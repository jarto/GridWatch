unit simulform;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Grids, StrUtils;

type
  TStrArray = array of String;

  { TGridWatchSimulationForm }

  TGridWatchSimulationForm = class(TForm)
    FileSaveDialog: TSaveDialog;
    SaveResults: TButton;
    FileOpenDialog: TOpenDialog;
    ShortageTreshold: TEdit;
    Label10: TLabel;
    Label9: TLabel;
    PumpStart: TRadioGroup;
    ShortagesGrid: TStringGrid;
    Simulate: TButton;
    Filter: TEdit;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    OpenCsv: TButton;
    StorageEfficiency: TEdit;
    Label6: TLabel;
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
    FStoredElectricity: Int64;
    FStorageEfficiency: Integer; // In percentage
    FShortageCount: Integer;
    FShortageStarted: TDateTime;
    FWorstShortage: Integer;
    FShortageTreshold: Integer;
    FMaxStored: Int64;
    FDemandColumn: Integer;
    FWindColumn: Integer;
    FHydroColumn: Integer;
    FTimeColumn: Integer;
    procedure FindColumns(const ColData: TStrArray);
    procedure LogEndOfShortage(TimeStamp: TDateTime);
    procedure StoreElectricity(e: Int64; TimeStamp: TDateTime);
    procedure UseStoredElectricity(e,Demand: Int64; TimeStamp: TDateTime);
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
    StartStore: Integer;

  procedure AddToStream(const Buf: AnsiString);
  begin
    FStream.Write(Buf[1],Length(Buf));
  end;

begin
  if FileSaveDialog.Execute then begin
    FStream:=TFileStream.Create(FileSaveDialog.FileName,fmCreate);
    try
      AddToStream(Format('Year %s',[Filter.Text])+LineEnding);
      AddToStream(Format('Shortage treshold %d %',[FShortageTreshold])+LineEnding);
      if UseWind.Checked then
        AddToStream(Format('Wind %s x current capacity',[WindCap.Text])+LineEnding);
      if UseHydro.Checked then
      AddToStream(Format('Hydro %s x current capacity',[HydroCap.Text])+LineEnding);
      if UsePump.Checked then begin;
        AddToStream(Format('Pumped storage %s x Dinorwig 1728 MW',[MaxPump.Text])+LineEnding);
        AddToStream(Format('Pumped storage efficiency %s %',[StorageEfficiency.Text])+LineEnding);
        case PumpStart.ItemIndex of
          1: StartStore:=50;  //Start half full
          2: StartStore:=100; //Start full capacity
          else StartStore:=0;
        end;
        AddToStream(Format('Capacity at start %d %',[StartStore])+LineEnding);
      end;
      AddToStream(LineEnding);
      ShortagesGrid.SaveToCSVStream(FStream);
    finally
      FStream.Free;
    end;
  end;
end;

procedure TGridWatchSimulationForm.FormDestroy(Sender: TObject);
begin

end;

procedure TGridWatchSimulationForm.SimulateClick(Sender: TObject);
var Demand,Production,Extra: Integer;
    ProdWind,ProdHydro: Integer;
    WindShare,HydroShare: Extended;
    ColData: array of String;
    Year,DataLine: String;
    F: TextFile;
    TimeStamp: TDateTime;
begin
  if FFileName='' then exit;

  FDemandColumn:=0;
  FWindColumn:=0;
  FHydroColumn:=0;
  FTimeColumn:=0;
  FShortageCount:=0;
  FShortageStarted:=0;

  FStorageEfficiency:=StrToIntDef(StorageEfficiency.Text,0);
  if FStorageEfficiency<0 then FStorageEfficiency:=0
  else if FStorageEfficiency>100 then FStorageEfficiency:=100;

  FShortageTreshold:=StrToIntDef(ShortageTreshold.Text,100);
  if FShortageTreshold>100 then FShortageTreshold:=100;
  if FShortageTreshold<0 then FShortageTreshold:=0;

  FWorstShortage:=100;
  if UsePump.Checked then FMaxStored:=Round(StrToFloatDef(MaxPump.Text,0)*1728) else FMaxStored:=0;
  if FMaxStored<0 then FMaxStored:=0;

  Year:=Filter.Text;

  if UseWind.Checked then WindShare:=StrToFloatDef(WindCap.Text,0) else WindShare:=0;
  if UseHydro.Checked then HydroShare:=StrToFloatDef(HydroCap.Text,0) else HydroShare:=0;

  case PumpStart.ItemIndex of
    1: FStoredElectricity:=FMaxStored div 2; //Start half full
    2: FStoredElectricity:=FMaxStored;       //Start full capacity
    else FStoredElectricity:=0;              //Start empty
  end;

  Screen.Cursor:=crHourGlass;
  try
    AssignFile(F,FFileName);
    SetLength(ColData,1);
    try
      Reset(F);
      while not eof(F) do begin
        ReadLn(F,DataLine);
        SplitString(DataLine,',',ColData);
        if FWindColumn=0 then begin
          FindColumns(ColData);
          continue;
        end;
        TimeStamp:=StrToDateTime(ColData[FTimeColumn]);
        if (Year<>'') and (pos(Year,ColData[FTimeColumn])<>1) then begin
          if FShortageStarted>0 then LogEndOfShortage(TimeStamp);
          continue;
        end;
        Demand:=StrToIntDef(ColData[FDemandColumn],0);
        ProdWind:=StrToIntDef(ColData[FWindColumn],0);
        ProdHydro:=StrToIntDef(ColData[FHydroColumn],0);
        Production:=Round(WindShare*ProdWind+HydroShare*ProdHydro);
        if (Demand<=0) or (Production<=0) then continue; //Probably en error in data -> Ignore
        Extra:=Production-Demand;
        if Extra>=0 then StoreElectricity(Extra,TimeStamp)
        else UseStoredElectricity(-Extra,Demand,TimeStamp);
      end;
      if FShortageStarted>0 then LogEndOfShortage(TimeStamp);
    finally
      CloseFile(F);
    end;
  finally
    Screen.Cursor:=crDefault;
  end;
  SaveResults.Enabled:=true;
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
    if FWorstShortage>FShortageTreshold then exit;
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
    i:=Trunc(ShLen/EncodeTime(1,0,0,0));
    if i>0 then begin
      ShortagesGrid.Cells[3,FShortageCount]:=IntToStr(i);
      ShLen:=ShLen-i*EncodeTime(1,0,0,0);
    end;
    i:=Trunc(ShLen/EncodeTime(0,1,0,0));
    if i>0 then begin
      ShortagesGrid.Cells[4,FShortageCount]:=IntToStr(i);
      ShLen:=ShLen-i*EncodeTime(0,1,0,0);
    end;
    i:=Trunc(ShLen/EncodeTime(0,0,1,0));
    if i>0 then begin
      ShortagesGrid.Cells[5,FShortageCount]:=IntToStr(i);
    end;
    ShortagesGrid.Cells[6,FShortageCount]:=IntToStr(FWorstShortage)+'%';
  finally
    FShortageStarted:=0;
    FWorstShortage:=100;
  end;
end;

procedure TGridWatchSimulationForm.StoreElectricity(e: Int64; TimeStamp: TDateTime);
begin
  if FShortageStarted>0 then LogEndOfShortage(TimeStamp);
  FStoredElectricity:=FStoredElectricity+e*FStorageEfficiency div 100;
  if FStoredElectricity>FMaxStored then FStoredElectricity:=FMaxStored;
end;

procedure TGridWatchSimulationForm.UseStoredElectricity(e,Demand: Int64; TimeStamp: TDateTime);
var Missing,Per: Int64;
begin
  Missing:=e-FStoredElectricity;
  if Missing<=0 then Missing:=0;
  if Missing>0 then begin
    Per:=100-Round((Missing*100/Demand));
    if Per<FWorstShortage then FWorstShortage:=Per;
  end;
  if FStoredElectricity>0 then begin
    FStoredElectricity:=FStoredElectricity-e;
    if FStoredElectricity>=0 then begin //Enough energy in storage
      if FShortageStarted>0 then LogEndOfShortage(TimeStamp);
      exit;
    end;
    //Not enough stored -> Start shortage
    FStoredElectricity:=0;
    FShortageStarted:=TimeStamp;
  end;
end;

end.

