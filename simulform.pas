unit simulform;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Grids, ComCtrls, StrUtils;

type
  TStrArray = array of String;

  { TGridWatchSimulationForm }

  TGridWatchSimulationForm = class(TForm)
    FileSaveDialog: TSaveDialog;
    Label11: TLabel;
    Label12: TLabel;
    ResultsMemo: TMemo;
    PageControl1: TPageControl;
    SaveResults: TButton;
    FileOpenDialog: TOpenDialog;
    ShortagesGrid: TStringGrid;
    ShortageTreshold: TEdit;
    Label10: TLabel;
    Label9: TLabel;
    PumpStart: TRadioGroup;
    Simulate: TButton;
    Filter: TEdit;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    OpenCsv: TButton;
    StorageEfficiency: TEdit;
    Label6: TLabel;
    StorageMaxHours: TEdit;
    ResultsPage: TTabSheet;
    ShortagesPage: TTabSheet;
    UsePump: TCheckBox;
    Label2: TLabel;
    MaxPump: TEdit;
    Storage: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    WindCap: TEdit;
    UseHydro: TCheckBox;
    UseWind: TCheckBox;
    EnergyForms: TGroupBox;
    HydroCap: TEdit;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OpenCsvClick(Sender: TObject);
    procedure SaveResultsClick(Sender: TObject);
    procedure SimulateClick(Sender: TObject);
  private
    FFileName: String;
    FCapMultiEdits: array of TEdit;
    FDinorwigCount: Extended;
    FStoredElectricity: Extended;
    FWastedElectricity: Extended;
    FUsedElectricity: Extended;
    FStorageEfficiency: Integer; // In percentage
    FStorageMaxTime: Integer; // In hours
    FShortageCount: Integer;
    FShortageStarted: TDateTime;
    FWorstShortage: Extended;
    FAllTimeWorstShortage: Extended;
    FShortageTreshold: Extended;
    FMaxStored: Extended;
    FDemandColumn: Integer;
    FWindColumn: Integer;
    FHydroColumn: Integer;
    FTimeColumn: Integer;
    function BorrowFromStorage(Needed: Extended): Extended;
    procedure FindColumns(const ColData: TStrArray);
    procedure LogEndOfShortage(TimeStamp: TDateTime);
    procedure SaveResultsToStream(Stream: TStream);
    procedure StoreElectricity(StoreMWh: Extended; TimeStamp: TDateTime);
  public
    { public declarations }
  end;

var
  GridWatchSimulationForm: TGridWatchSimulationForm;

implementation

{$R *.lfm}

procedure SplitString(const S, Delimiter: string; var Parts: TStrArray);
var
  i,PatLength,P,Count,ColCount: Integer;

  procedure AddColumn(const C: String);
  begin
    inc(Count);
    if Count>ColCount then begin
      ColCount:=Count;
      SetLength(Parts,ColCount);
    end;
    Parts[Count-1]:=Trim(C);
  end;

begin
  PatLength:=Length(Delimiter);
  ColCount:=Length(Parts);
  P:=1; Count:=0;
  repeat
    i:=PosEx(Delimiter,S,P);
    if i>0 then begin
      AddColumn(copy(S,P,i-P));
      P:=i+PatLength;
    end;
  until i=0;
  if P<=Length(S) then AddColumn(copy(S,P,Length(S)));
end;


{ TGridWatchSimulationForm }

procedure TGridWatchSimulationForm.FormShow(Sender: TObject);
begin
  FormatSettings.DateSeparator:='-';
  FormatSettings.ShortDateFormat:='y/m/d';
end;

procedure TGridWatchSimulationForm.OpenCsvClick(Sender: TObject);
begin
  FileOpenDialog.Title:='Open Gridwatch csv file';
  if FileOpenDialog.Execute then begin
    FFileName:=FileOpenDialog.FileName;
    Simulate.Enabled:=True;
  end;
end;

procedure TGridWatchSimulationForm.SaveResultsClick(Sender: TObject);
var FStream: TFileStream;
begin
  if FileSaveDialog.Execute then begin
    FStream:=TFileStream.Create(FileSaveDialog.FileName+'-info.txt',fmCreate);
    try
      SaveResultsToStream(FStream);
    finally
      FStream.Free;
    end;
    ShortagesGrid.SaveToCSVFile(FileSaveDialog.FileName);
  end;
end;

procedure TGridWatchSimulationForm.FormDestroy(Sender: TObject);
begin

end;

procedure TGridWatchSimulationForm.SimulateClick(Sender: TObject);
var Demand,Production: Integer;
    ProdWind,ProdHydro: Integer;
    WindShare,HydroShare,OneHour,eMax: Extended;
    ColData: array of String;
    Year,DataLine: String;
    F: TextFile;
    TimeStamp,LastTimeStamp,LastDt: TDateTime;
    TempStream: TStringStream;

  procedure Calculate(dT: TDateTime);
  var DemandMWh,ProductionMWh,ExtraMWh,NeededMWh,AvailableMWh,Percentage: Extended;
  begin
    LastdT:=dT;
    //Convert to MWh
    ProductionMWh:=Production*dT/OneHour;
    DemandMWh:=Demand*dT/OneHour;
    ExtraMWh:=ProductionMWh-DemandMWh;
    FUsedElectricity:=FUsedElectricity+DemandMWh;
    if ExtraMWh>=0 then begin
      StoreElectricity(ExtraMWh,TimeStamp);
    end else begin
      NeededMWh:=-ExtraMWh;
      eMax:=FDinorwigCount*1728*dT/OneHour; //Max amount available from pumped storage during dT
      if NeededMWh>eMax then AvailableMWh:=BorrowFromStorage(eMax)
      else AvailableMWh:=BorrowFromStorage(NeededMWh);
      Percentage:=100*(ProductionMWh+AvailableMWh)/DemandMWh;
      if Percentage<=FShortageTreshold then begin
        //Not enough
        if Percentage<FWorstShortage then FWorstShortage:=Percentage;
        if Percentage<FAllTimeWorstShortage then FAllTimeWorstShortage:=Percentage;
        //Log a shortage
        if FShortageStarted=0 then FShortageStarted:=TimeStamp;
      end else begin
        //Not enough but over shortage treshold
        if FShortageStarted>0 then LogEndOfShortage(TimeStamp);
      end;
    end;
  end;

begin
  if FFileName='' then exit;
  ShortagesGrid.RowCount:=1;

  FDemandColumn:=0;
  FWindColumn:=0;
  FHydroColumn:=0;
  FTimeColumn:=0;
  FShortageCount:=0;
  FShortageStarted:=0;
  FWastedElectricity:=0;
  FUsedElectricity:=0;
  FWorstShortage:=100;
  FAllTimeWorstShortage:=100;
  OneHour:=EncodeTime(1,0,0,0);

  //Efficiency of storage. Allowed values 0-100
  FStorageEfficiency:=StrToIntDef(StorageEfficiency.Text,0);
  if FStorageEfficiency<0 then FStorageEfficiency:=0
  else if FStorageEfficiency>100 then FStorageEfficiency:=100;

  //How many hours the pumped storage can produce at max efficiency
  FStorageMaxTime:=Round(StrToFloatDef(StorageMaxHours.Text,0));
  if FStorageMaxTime<0 then FStorageMaxTime:=0;

  //If production drops under this, a shortage is logged. Allowed values 0-100.
  FShortageTreshold:=StrToFloatDef(ShortageTreshold.Text,100);
  if FShortageTreshold>100 then FShortageTreshold:=100;
  if FShortageTreshold<0 then FShortageTreshold:=0;

  if UsePump.Checked then begin
    FDinorwigCount:=StrToFloatDef(MaxPump.Text,0); //How many Dinorwigs do we have?
    if FDinorwigCount<0 then FDinorwigCount:=0;
    FMaxStored:=FDinorwigCount*1728*FStorageMaxTime; //Max energy that can be stored
    if FMaxStored<0 then FMaxStored:=0;
  end else begin
    FMaxStored:=0;
    FDinorwigCount:=0;
    eMax:=0;
  end;

  //Stored energy at start of simulation
  case PumpStart.ItemIndex of
    1: FStoredElectricity:=FMaxStored*0.5;   //Start half full
    2: FStoredElectricity:=FMaxStored;       //Start full capacity
    else FStoredElectricity:=0;              //Start empty
  end;

  //How much wind and hydro will be used
  if UseWind.Checked then WindShare:=StrToFloatDef(WindCap.Text,0) else WindShare:=0;
  if UseHydro.Checked then HydroShare:=StrToFloatDef(HydroCap.Text,0) else HydroShare:=0;

  Year:=Filter.Text;

  Screen.Cursor:=crHourGlass;
  try
    AssignFile(F,FFileName);
    SetLength(ColData,1);
    LastTimeStamp:=0; Production:=0;
    TimeStamp:=0; Demand:=0; LastdT:=0;
    try
      Reset(F);
      while not eof(F) do begin
        ReadLn(F,DataLine);
        SplitString(DataLine,',',ColData);
        //Find the right columns from column headers
        if FWindColumn=0 then begin
          FindColumns(ColData);
          continue;
        end;
        TimeStamp:=StrToDateTime(ColData[FTimeColumn]);
        if LastTimeStamp>0 then Calculate(TimeStamp-LastTimeStamp);
        LastTimeStamp:=TimeStamp;
        Demand:=StrToIntDef(ColData[FDemandColumn],0);
        ProdWind:=StrToIntDef(ColData[FWindColumn],0);
        ProdHydro:=StrToIntDef(ColData[FHydroColumn],0);
        Production:=Round(WindShare*ProdWind+HydroShare*ProdHydro);
        if (Year<>'') and (pos(Year,ColData[FTimeColumn])<>1) then begin
          if LastTimeStamp>0 then Calculate(TimeStamp-LastTimeStamp)
          else if  LastdT>0 then Calculate(LastdT);
          if FShortageStarted>0 then LogEndOfShortage(TimeStamp);
          LastdT:=0; LastTimeStamp:=0;
        end;
      end;
      if FShortageStarted>0 then LogEndOfShortage(TimeStamp);
    finally
      CloseFile(F);
    end;
    ResultsMemo.Clear;
    TempStream:=TStringStream.Create('');
    try
      SaveResultsToStream(TempStream);
      ResultsMemo.Text:=TempStream.DataString;
    finally
      TempStream.Free;
    end;
  finally
    Screen.Cursor:=crDefault;
  end;
  SaveResults.Enabled:=true;
end;

function TGridWatchSimulationForm.BorrowFromStorage(Needed: Extended): Extended;
begin
  if Needed<FStoredElectricity then begin
    result:=Needed;
    FStoredElectricity:=FStoredElectricity-Needed;
  end else begin
    result:=FStoredElectricity;
    FStoredElectricity:=0;
  end;
end;

procedure TGridWatchSimulationForm.FindColumns(const ColData: TStrArray);
var a: Integer;
begin
  for a:=0 to High(ColData) do begin
    if ColData[a]='timestamp' then FTimeColumn:=a
    else if ColData[a]='demand' then FDemandColumn:=a
    else if ColData[a]='wind' then FWindColumn:=a
    else if ColData[a]='hydro' then FHydroColumn:=a;
  end;
end;

procedure TGridWatchSimulationForm.LogEndOfShortage(TimeStamp: TDateTime);
var ShLen: TDateTime;
    i: Integer;
begin
  try
    ShLen:=TimeStamp-FShortageStarted;
    inc(FShortageCount);
    ShortagesGrid.RowCount:=FShortageCount+1;
    ShortagesGrid.Cells[0,FShortageCount]:=FormatDateTime('dd.mm.yyyy hh:nn:ss',FShortageStarted);
    ShortagesGrid.Cells[1,FShortageCount]:=FormatDateTime('dd.mm.yyyy hh:nn:ss',TimeStamp);
    i:=Trunc(ShLen);
    if i>0 then begin
      ShortagesGrid.Cells[2,FShortageCount]:=IntToStr(i);
      ShLen:=ShLen-i;
    end;
    //Calculate days
    i:=Trunc(ShLen/EncodeTime(1,0,0,0));
    if i>0 then begin
      ShortagesGrid.Cells[3,FShortageCount]:=IntToStr(i);
      ShLen:=ShLen-i*EncodeTime(1,0,0,0);
    end;
    //Hours
    i:=Trunc(ShLen/EncodeTime(0,1,0,0));
    if i>0 then begin
      ShortagesGrid.Cells[4,FShortageCount]:=IntToStr(i);
      ShLen:=ShLen-i*EncodeTime(0,1,0,0);
    end;
    //Minutes
    i:=Trunc(ShLen/EncodeTime(0,0,1,0));
    if i>0 then begin
      ShortagesGrid.Cells[5,FShortageCount]:=IntToStr(i);
    end;
    ShortagesGrid.Cells[6,FShortageCount]:=IntToStr(Round(FWorstShortage))+'%';
  finally
    FShortageStarted:=0;
    FWorstShortage:=100;
  end;
end;

procedure TGridWatchSimulationForm.SaveResultsToStream(Stream: TStream);
var StartStore: Extended;

  procedure AddToStream(const Buf: AnsiString);
  begin
    Stream.Write(Buf[1],Length(Buf));
  end;

begin
  AddToStream(Format('Year %s',[Filter.Text])+LineEnding);
  AddToStream(Format('Shortage treshold %n',[FShortageTreshold])+'%'+LineEnding);
  if UseWind.Checked then
    AddToStream(Format('Wind %s x current capacity',[WindCap.Text])+LineEnding);
  if UseHydro.Checked then
  AddToStream(Format('Hydro %s x current capacity',[HydroCap.Text])+LineEnding);
  if UsePump.Checked then begin;
    AddToStream(Format('Pumped storage %s x Dinorwig 1728 MW',[MaxPump.Text])+LineEnding);
    AddToStream(Format('Pumped storage efficiency %s',[StorageEfficiency.Text])+'%'+LineEnding);
    case PumpStart.ItemIndex of
      1: StartStore:=FMaxStored/2;  //Start half full
      2: StartStore:=FMaxStored;    //Start full capacity
      else StartStore:=0;
    end;
    AddToStream(Format('Stored electricity at start %n MWh',[StartStore])+LineEnding);
    AddToStream(Format('Stored electricity at end %n MWh',[FStoredElectricity])+LineEnding);
    AddToStream(Format('Used electricity %n MWh',[FUsedElectricity])+LineEnding);
    AddToStream(Format('Wasted electricity %n MWh',[FWastedElectricity])+LineEnding);
    AddToStream(Format('Shortages: %d',[ShortagesGrid.RowCount-1])+LineEnding);
    AddToStream(Format('Worst shortage %n',[FAllTimeWorstShortage])+'%'+LineEnding);
  end;
end;

procedure TGridWatchSimulationForm.StoreElectricity(StoreMWh: Extended; TimeStamp: TDateTime);
//  Store exlectricity until the pumped storage is full.
var WithEfficiency: Extended;
begin
  if FShortageStarted>0 then LogEndOfShortage(TimeStamp);
  //What is left when storage efficiency is counted
  WithEfficiency:=StoreMWh*FStorageEfficiency/100;
  //Difference is added to wasted electricity
  FWastedElectricity:=FWastedElectricity+StoreMWh-WithEfficiency;
  FStoredElectricity:=FStoredElectricity+WithEfficiency;
  //If storage is full, add the difference to wasted electricity
  if FStoredElectricity>FMaxStored then begin
    FWastedElectricity:=FWastedElectricity+FStoredElectricity-FMaxStored;
    FStoredElectricity:=FMaxStored;
  end;
end;

end.

