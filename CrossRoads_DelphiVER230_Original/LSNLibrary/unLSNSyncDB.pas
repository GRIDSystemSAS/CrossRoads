//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
// TLSNSyncDB v.1.0
// by Laurent Sengmany - 30.11.2011
//-----------------------------------------------------------------------------
// Description : this class will help you to synchronize (load and save) your
//               registered clientdatasets to a directory
// How-to :
//      // create sync and register clientdatasets to be 'synchronized':
//      FLSNSyncDB := TLSNSyncDB.Create;
//      FLSNSyncDB.RegisterDataset(cdsParameters, 'Name');
//      // initialize a sync with a directory
//      FLSNSyncDB.Initialize(FDataDirectory);
//      // load registered clientdatasets from directory:
//      FLSNSyncDB.load;
//      // save registered clientdatasets to directory:
//      FMyData.Save;
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
unit unLSNSyncDB;

interface

uses
  System.SysUtils, System.Classes, IOUtils, Data.DB, Datasnap.DBClient;

type
  TLSNSyncDB = class
  private
    FDatasetList: TList;
    FDatasetNameList: TStringList;
    FDirectory: string;
    FSaveFormat: TDataPacketFormat;
    //
    procedure SaveDataset(aClientDataset: TClientDataset; aSaveName: string);
  protected
    //
  public
    constructor Create;
    destructor Destroy; override;
    procedure ReInit;
    //
    procedure Initialize(aDirectory: string; aIsSaveFormatToXML: Boolean= False);
    procedure RegisterDataset(aClientDataset: TClientDataset; aSaveName: string);
    procedure Load;
    procedure Save;
    //
    property Directory: string read FDirectory;
    property SaveFormat: TDataPacketFormat read FSaveFormat write FSaveFormat;
  end;

implementation

{$REGION 'TLSNSyncDB'}

constructor TLSNSyncDB.Create;
begin
  FDatasetList := TList.Create;
  FDatasetNameList := TStringList.Create;
end;

destructor TLSNSyncDB.Destroy;
begin
  FDatasetList.Free;
  FDatasetNameList.Free;
end;

procedure TLSNSyncDB.ReInit;
begin
  FDatasetList.Clear;
  FDatasetNameList.Clear;
end;

procedure TLSNSyncDB.Initialize(aDirectory: string; aIsSaveFormatToXML: Boolean= False);
begin
  FDirectory := aDirectory;
  if not TDirectory.Exists(FDirectory) then
    TDirectory.CreateDirectory(FDirectory);
  //
  if aIsSaveFormatToXML then
    FSaveFormat := TDataPacketFormat.dfXMLUTF8
  else
    FSaveFormat := TDataPacketFormat.dfBinary;
end;

procedure TLSNSyncDB.RegisterDataset(aClientDataset: TClientDataset; aSaveName: string);
begin
  FDatasetList.Add(aClientDataset);
  FDatasetNameList.Add(aSaveName);
end;

procedure TLSNSyncDB.Load;
var
  lClientDB: TClientDataset;
  i: integer;
  lFileName: string;
begin
  for i := 0 to FDatasetList.Count-1 do
  begin
    lClientDB := FDatasetList.Items[i];
    lClientDB.Close;
    //
    lFileName := FDirectory + TPath.DirectorySeparatorChar + FDatasetNameList[i];
    if TFile.Exists(lFileName,True) then
      lClientDB.LoadFromFile(lFileName)
    else
      lClientDB.CreateDataSet;
    //
    lClientDB.LogChanges := False;
  end;
end;

procedure TLSNSyncDB.SaveDataset(aClientDataset: TClientDataset; aSaveName: string);
var
  lFileName: string;
begin
  lFileName := FDirectory + TPath.DirectorySeparatorChar + aSaveName;
  aClientDataset.SaveToFile(lFileName, FSaveFormat);
end;

procedure TLSNSyncDB.Save;
var
  lClientDB: TClientDataset;
  i: integer;
begin
  for i := 0 to FDatasetList.Count-1 do
  begin
    lClientDB := FDatasetList.Items[i];
    SaveDataset(lClientDB, FDatasetNameList[i]);
  end;
end;

{$ENDREGION}

end.
