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
    NuclearCap: TEdit;
    FileSaveDialog: TSaveDialog;
    BioCap: TEdit;
    Label14: TLabel;
    Label15: TLabel;
    Label6: TLabel;
    SolarCap: TEdit;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    ResultsMemo: TMemo;
    PageControl1: TPageControl;
    SaveResults: TButton;
    FileOpenDialog: TOpenDialog;
    ShortagesGrid: TStringGrid;
    ShortageTreshold: TEdit;
    Label10: TLabel;
    Label9: TLabel;
    StorageStart: TRadioGroup;
    Simulate: TButton;
    Filter: TEdit;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    OpenCsv: TButton;
    StorageEfficiency: TEdit;
    StorageMaxHours: TEdit;
    ResultsPage: TTabSheet;
    ShortagesPage: TTabSheet;
    UseBiomass: TCheckBox;
    UseNuclear: TCheckBox;
    UseSolar: TCheckBox;
    UseTeslaBattery: TCheckBox;
    Label2: TLabel;
    MaxBatteries: TEdit;
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
    FStorageCount: Extended;
    FStoredElectricity: Extended;
    FWastedElectricity: Extended;
    FUsedElectricity: Extended;
    FWindTotal: Extended;
    FSolarTotal: Extended;
    FHydroTotal: Extended;
    FBioTotal: Extended;
    FNuclearTotal: Extended;
    FStorageEfficiency: Integer; // In percentage
    FStorageMaxTime: Integer; // In hours
    FShortageCount: Integer;
    FShortageStarted: TDateTime;
    FWorstShortage: Extended;
    FLowestStorage: Extended;
    FAllTimeWorstShortage: Extended;
    FShortageTreshold: Extended;
    FMaxStored: Extended;
    FDemandColumn: Integer;
    FWindColumn: Integer;
    FSolarColumn: Integer;
    FHydroColumn: Integer;
    FBioColumn: Integer;
    FNuclearColumn: Integer;
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
    ProdWind,ProdSolar,ProdHydro,ProdBio,ProdNuclear: Extended;
    WindShare,SolarShare,HydroShare,BioShare,NuclearShare,OneHour,eMax: Extended;
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

    FWindTotal:=FWindTotal+WindShare*ProdWind*dT/OneHour;
    FSolarTotal:=FSolarTotal+SolarShare*ProdSolar*dT/OneHour;
    FHydroTotal:=FHydroTotal+HydroShare*ProdHydro*dT/OneHour;
    FBioTotal:=FBioTotal+BioShare*ProdBio*dT/OneHour;
    FNuclearTotal:=FNuclearTotal+NuclearShare*ProdNuclear*dT/OneHour;

    if ExtraMWh>=0 then begin
      StoreElectricity(ExtraMWh,TimeStamp);
      if FShortageStarted>0 then LogEndOfShortage(TimeStamp);
    end else begin
      NeededMWh:=-ExtraMWh;
      eMax:=FStorageCount*150*dT/OneHour; //Max amount available from storage during dT
      if NeededMWh>eMax then AvailableMWh:=BorrowFromStorage(eMax)
      else AvailableMWh:=BorrowFromStorage(NeededMWh);
      Percentage:=Round(100*(ProductionMWh+AvailableMWh)/DemandMWh);
      if Percentage<FShortageTreshold then begin
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
  FSolarColumn:=0;
  FHydroColumn:=0;
  FBioColumn:=0;
  FNuclearColumn:=0;
  FTimeColumn:=0;
  FShortageCount:=0;
  FShortageStarted:=0;
  FWastedElectricity:=0;
  FUsedElectricity:=0;
  FWindTotal:=0;
  FSolarTotal:=0;
  FHydroTotal:=0;
  FBioTotal:=0;
  FNuclearTotal:=0;
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

  if UseTeslaBattery.Checked then begin
    FStorageCount:=StrToFloatDef(MaxBatteries.Text,0); //How many Big Batteries do we have?
    if FStorageCount<0 then FStorageCount:=0;
    FMaxStored:=FStorageCount*150*FStorageMaxTime; //Max energy that can be stored
    if FMaxStored<0 then FMaxStored:=0;
  end else begin
    FMaxStored:=0;
    FStorageCount:=0;
    eMax:=0;
  end;

  //Stored energy at start of simulation
  case StorageStart.ItemIndex of
    1: FStoredElectricity:=FMaxStored*0.5;   //Start half full
    2: FStoredElectricity:=FMaxStored;       //Start full capacity
    else FStoredElectricity:=0;              //Start empty
  end;

  FLowestStorage:=100;

  //How much wind, solar, hydro, biomass and nuclear will be used
  if UseWind.Checked then WindShare:=StrToFloatDef(WindCap.Text,0) else WindShare:=0;
  if UseSolar.Checked then SolarShare:=StrToFloatDef(SolarCap.Text,0) else SolarShare:=0;
  if UseHydro.Checked then HydroShare:=StrToFloatDef(HydroCap.Text,0) else HydroShare:=0;
  if UseBiomass.Checked then BioShare:=StrToFloatDef(BioCap.Text,0) else BioShare:=0;
  if UseNuclear.Checked then NuclearShare:=StrToFloatDef(NuclearCap.Text,0) else NuclearShare:=0;

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
        if FTimeColumn=0 then begin
          FindColumns(ColData);
          continue;
        end;
        if (Year='') or (pos(Year,ColData[FTimeColumn])<>0) then begin
          TimeStamp:=StrToDateTime(ColData[FTimeColumn]);
          //if LastTimeStamp>0 then Calculate(TimeStamp-LastTimeStamp);
          //LastTimeStamp:=TimeStamp;
          Demand:=StrToIntDef(ColData[FDemandColumn],0);
          ProdWind:=StrToIntDef(ColData[FWindColumn],0);
          if FSolarColumn>0 then ProdSolar:=StrToFloatDef(ColData[FSolarColumn],0) else ProdSolar:=0;
          if FHydroColumn>0 then ProdHydro:=StrToIntDef(ColData[FHydroColumn],0) else ProdHydro:=0;
          if FBioColumn>0 then ProdBio:=StrToIntDef(ColData[FBioColumn],0) else ProdBio:=0;
          if FNuclearColumn>0 then ProdNuclear:=StrToIntDef(ColData[FNuclearColumn],0) else ProdNuclear:=0;
          Production:=Round(WindShare*ProdWind
                           +SolarShare*ProdSolar
                           +HydroShare*ProdHydro
                           +BioShare*ProdBio
                           +NuclearShare*ProdNuclear);
          if LastTimeStamp>0 then Calculate(TimeStamp-LastTimeStamp)
          else if LastdT>0 then Calculate(LastdT);
          LastTimeStamp:=TimeStamp;
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
var LeftInStorage: Extended;
begin
  if Needed<FStoredElectricity then begin
    result:=Needed;
    FStoredElectricity:=FStoredElectricity-Needed;
    LeftInStorage:=Round(100*FStoredElectricity/FMaxStored);
    if LeftInStorage<FLowestStorage then FLowestStorage:=LeftInStorage;
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
    else if ColData[a]='solar' then FSolarColumn:=a
    else if ColData[a]='hydro' then FHydroColumn:=a
    else if ColData[a]='biomass' then FBioColumn:=a
    else if ColData[a]='nuclear' then FNuclearColumn:=a;
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
  if UseSolar.Checked then
    AddToStream(Format('Solar %s x current capacity',[SolarCap.Text])+LineEnding);
  if UseHydro.Checked then
    AddToStream(Format('Hydro %s x current capacity',[HydroCap.Text])+LineEnding);
  if UseBiomass.Checked then
    AddToStream(Format('Biomass %s x current capacity',[BioCap.Text])+LineEnding);
  if UseNuclear.Checked then
    AddToStream(Format('Nuclear %s x current capacity',[NuclearCap.Text])+LineEnding);

  AddToStream(LineEnding);
  AddToStream(Format('Total consumption %n MWh',[FUsedElectricity])+LineEnding);
  AddToStream(Format('Total production: %n MWh',[FWindTotal+FSolarTotal+FHydroTotal+FBioTotal+FNuclearTotal])+LineEnding);

  if FWindTotal>0 then
    AddToStream(Format('  Wind production: %n MWh',[FWindTotal])+LineEnding);
  if FSolarTotal>0 then
    AddToStream(Format('  Solar production: %n MWh',[FSolarTotal])+LineEnding);
  if FHydroTotal>0 then
    AddToStream(Format('  Hydro production: %n MWh',[FHydroTotal])+LineEnding);
  if FBioTotal>0 then
    AddToStream(Format('  Biomass production: %n MWh',[FBioTotal])+LineEnding);
  if FNuclearTotal>0 then
    AddToStream(Format('  Nuclear production: %n MWh',[FNuclearTotal])+LineEnding);

  if UseTeslaBattery.Checked then begin;
    AddToStream(LineEnding);
    AddToStream(Format('Battery storage %s x Tesla Big Battery 150 MW',[MaxBatteries.Text])+LineEnding);
    AddToStream(Format('Storage efficiency %s',[StorageEfficiency.Text])+'%'+LineEnding);
    case StorageStart.ItemIndex of
      1: StartStore:=FMaxStored/2;  //Start half full
      2: StartStore:=FMaxStored;    //Start full capacity
      else StartStore:=0;
    end;
    AddToStream(Format('Stored electricity at start %n MWh',[StartStore])+LineEnding);
    AddToStream(Format('Stored electricity at end %n MWh',[FStoredElectricity])+LineEnding);
    AddToStream(Format('Wasted electricity %n MWh',[FWastedElectricity])+LineEnding);
    AddToStream(LineEnding);
    AddToStream(Format('Shortages: %d',[ShortagesGrid.RowCount-1])+LineEnding);
    AddToStream(Format('Worst shortage %n',[FAllTimeWorstShortage])+'%'+LineEnding);
    AddToStream(Format('Lowest storage %n',[FLowestStorage])+'%'+LineEnding);
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

