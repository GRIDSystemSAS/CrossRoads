unit dmUserLog;

interface

uses
  System.SysUtils, System.Classes, Data.DB, Datasnap.DBClient, unLSNSyncDB,
  unStructure;

type
  TdtmUserLog = class(TDataModule)
    cdsUserLog: TClientDataSet;
    cdsUserLogLogID: TStringField;
    cdsUserLogCreationDate: TDateTimeField;
    cdsUserLogDescription: TStringField;
    cdsUserLogLogType: TIntegerField;
    cdsUserLogModificationDate: TDateTimeField;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FLSNSyncDB: TLSNSyncDB;
    FInitialized: Boolean;
  public
    procedure Initialize(aDirectory: string);
    procedure Load;
    procedure Save;
    procedure AddUserLog(aLogType: integer; aDescription: string);
    //
    function GetUserLog(aLogID: string; var aUserLogRec: TUserLogRecord): Boolean;
    procedure UpdateUserLog(aUserLogRec: TUserLogRecord);
    //
    property Initialized: Boolean read FInitialized;
  end;

var
  dtmUserLog: TdtmUserLog;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

procedure TdtmUserLog.AddUserLog(aLogType: integer; aDescription: string);
var
  lUserLogRec: TUserLogRecord;
begin
  lUserLogRec.LogID := FormatDatetime('yyyymmddhhnnsszzz',now);
  lUserLogRec.LogType := aLogType;
  lUserLogRec.Description := aDescription;
  lUserLogRec.CreationDate := now;
  lUserLogRec.ModificationDate := now;
  UpdateUserLog(lUserLogRec);
end;

procedure TdtmUserLog.DataModuleCreate(Sender: TObject);
begin
  FLSNSyncDB := TLSNSyncDB.Create;
  FInitialized := False;
end;

procedure TdtmUserLog.DataModuleDestroy(Sender: TObject);
begin
  FLSNSyncDB.Destroy;
end;

procedure TdtmUserLog.Initialize(aDirectory: string);
begin
  FLSNSyncDB.RegisterDataset(cdsUserLog, 'UserLog.cds');
  FLSNSyncDB.Initialize(aDirectory, True);
  FInitialized := True;
end;

procedure TdtmUserLog.Load;
begin
  FLSNSyncDB.Load;
end;

procedure TdtmUserLog.Save;
begin
  cdsUserLog.Filtered := False;
  FLSNSyncDB.Save;
end;

function TdtmUserLog.GetUserLog(aLogID: string; var aUserLogRec: TUserLogRecord): Boolean;
begin
  result := False;
  if cdsUserLog.FindKey([aLogID]) then
  begin
    aUserLogRec.LogID := cdsUserLog.FieldByName('LogID').AsString;
    aUserLogRec.Description := cdsUserLog.FieldByName('Description').AsString;
    aUserLogRec.LogType := cdsUserLog.FieldByName('LogType').AsInteger;
    aUserLogRec.CreationDate := cdsUserLog.FieldByName('CreationDate').AsDatetime;
    aUserLogRec.ModificationDate := cdsUserLog.FieldByName('ModificationDate').AsDatetime;
    result := True;
  end;
end;

procedure TdtmUserLog.UpdateUserLog(aUserLogRec: TUserLogRecord);
begin
  cdsUserLog.Filtered := False;
  if cdsUserLog.FindKey([aUserLogRec.LogID]) then
  begin
    cdsUserLog.Edit;
    cdsUserLog.FieldByName('Description').AsString := aUserLogRec.Description;
    cdsUserLog.FieldByName('ModificationDate').AsDatetime := aUserLogRec.ModificationDate;
    cdsUserLog.Post;
  end
  else
  begin
    cdsUserLog.Insert;
    cdsUserLog.FieldByName('LogID').AsString := aUserLogRec.LogID;
    cdsUserLog.FieldByName('LogType').AsInteger := aUserLogRec.LogType;
    cdsUserLog.FieldByName('Description').AsString := aUserLogRec.Description;
    cdsUserLog.FieldByName('CreationDate').AsDatetime := aUserLogRec.CreationDate;
    cdsUserLog.FieldByName('ModificationDate').AsDatetime := aUserLogRec.ModificationDate;
    cdsUserLog.Post;
  end;
end;

end.
