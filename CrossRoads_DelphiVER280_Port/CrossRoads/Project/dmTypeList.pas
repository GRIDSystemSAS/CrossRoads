unit dmTypeList;

interface

uses
  System.SysUtils, System.Classes, Data.DB, Datasnap.DBClient, unLSNSyncDB,
  unStructure;

type
  TdtmTypeList = class(TDataModule)
    cdsProjectStatus: TClientDataSet;
    cdsProjectStatusType: TIntegerField;
    cdsProjectStatusLabel: TStringField;
    cdsSprint: TClientDataSet;
    cdsSprintSprintCode: TStringField;
    cdsSprintYear: TIntegerField;
    cdsSprintWeekNum: TIntegerField;
    cdsSprintBeginDate: TDateTimeField;
    cdsSprintEndDate: TDateTimeField;
    cdsSprintCapacity: TIntegerField;
    cdsSprintPlanned: TIntegerField;
    cdsSprintConsumed: TIntegerField;
    cdsPriority: TClientDataSet;
    cdsPriorityType: TIntegerField;
    cdsPriorityLabel: TStringField;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FLSNSyncDB: TLSNSyncDB;
    procedure AddType(aClientDataset: TClientDataSet; aType: integer; aLabel: string);
    procedure CreateLocalTypeList;
    function GetTypeLabel(aClientDataset: TClientDataSet; aType: integer): string;

  public
    procedure Initialize(aDirectory: string);
    procedure Load;
    procedure Save;
    // Type functions
    function GetPriorityLabel(aType: integer): string;
    function GetStatusLabel(aType: integer): string;
    // Sprint functions
    procedure CheckSprintParameters;
    function GetSprint(aSprintCode: string; var aSprintRec: TSprintRecord): Boolean;
  end;

var
  dtmTypeList: TdtmTypeList;

const
  // Status
  staDeleted = 0;
  staArchived = 1;
  staCreated = 10;
  staValidated = 20;
  staScheduled = 30;
  staInProgress = 40;
  staDone = 50;
  // Priority
  priNotDefined = 0;
  priVeryLow = 1;
  priLow = 2;
  priMedium = 3;
  priHigh = 4;
  priVeryHigh = 5;
  priCritical = 6;
  // TaskType
  tstTarget = 1;
  tstTask = 2;
  tstComment = 3;
  tstLink = 4;

implementation

uses DateUtils, StrUtils, unGlobal;

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

procedure TdtmTypeList.AddType(aClientDataset: TClientDataSet; aType: integer; aLabel: string);
begin
  aClientDataset.Insert;
  aClientDataset.FieldByName('Type').AsInteger := aType;
  aClientDataset.FieldByName('Label').AsString := aLabel;
  aClientDataset.Post;
end;

procedure TdtmTypeList.CheckSprintParameters;
var
  lYear: integer;
  lStrID: string;

  procedure CreateSprintsOf(aYear: integer);
  var
    i: integer;
    lStartDate, lEndDate: TDatetime;
    lID: string;
  begin
    for i := 1 to 52 do
    begin
      lID := IntToStr(aYear)+'-'+RightStr('0'+IntToStr(i),2);
      lStartDate := StartOfAWeek(aYear, i, 1);
      lEndDate := EndOfAWeek(aYear, i, 7);
      //
      cdsSprint.Insert;
      cdsSprint.FieldByName('SprintCode').AsString := lID;
      cdsSprint.FieldByName('Year').AsInteger := aYear;
      cdsSprint.FieldByName('WeekNum').AsInteger := i;
      cdsSprint.FieldByName('BeginDate').AsDatetime := lStartDate;
      cdsSprint.FieldByName('EndDate').AsDatetime := lEndDate;
      cdsSprint.FieldByName('Capacity').AsInteger := MyParams.ParamInteger[prmWeekCapacity];
      cdsSprint.FieldByName('Planned').AsInteger := 0;
      cdsSprint.FieldByName('Consumed').AsInteger := 0;
      cdsSprint.Post;
    end;
  end;
begin
  lYear := YearOf(date)-1;
  lStrID := IntToStr(lYear)+'-01';
  if not cdsSprint.FindKey([lStrID]) then
    CreateSprintsOf(lYear);
  //
  lYear := YearOf(date);
  lStrID := IntToStr(lYear)+'-01';
  if not cdsSprint.FindKey([lStrID]) then
    CreateSprintsOf(lYear);
  //
  lYear := lYear+1;
  lStrID := IntToStr(lYear)+'-01';
  if not cdsSprint.FindKey([lStrID]) then
    CreateSprintsOf(lYear);
end;

procedure TdtmTypeList.CreateLocalTypeList;
begin
  // Project status
  cdsProjectStatus.CreateDataSet;
  AddType(cdsProjectStatus, staDeleted, 'Deleted');
  AddType(cdsProjectStatus, staArchived, 'Archived');
  AddType(cdsProjectStatus, staCreated, 'Created');
  AddType(cdsProjectStatus, staValidated, 'Validated');
  AddType(cdsProjectStatus, staScheduled, 'Scheduled');
  AddType(cdsProjectStatus, staInProgress, 'In Progress');
  AddType(cdsProjectStatus, staDone, 'Done');
  // Priority
  cdsPriority.CreateDataSet;
  AddType(cdsPriority, priNotDefined, 'Not defined (0)');
  AddType(cdsPriority, priVeryLow, 'Very low (1)');
  AddType(cdsPriority, priLow, 'Low (2)');
  AddType(cdsPriority, priMedium, 'Medium (3)');
  AddType(cdsPriority, priHigh, 'High (4)');
  AddType(cdsPriority, priVeryHigh, 'Very high (5)');
  AddType(cdsPriority, priCritical, 'Critical (6)');

end;

function TdtmTypeList.GetPriorityLabel(aType: integer): string;
begin
  result := GetTypeLabel(cdsPriority, aType);
end;

function TdtmTypeList.GetStatusLabel(aType: integer): string;
begin
  result := GetTypeLabel(cdsProjectStatus, aType);
end;

function TdtmTypeList.GetSprint(aSprintCode: string; var aSprintRec: TSprintRecord): Boolean;
begin
  result := False;
  if cdsSprint.FindKey([aSprintCode]) then
  begin
    aSprintRec.SprintCode := cdsSprint.FieldByName('SprintCode').AsString;
    aSprintRec.Year := cdsSprint.FieldByName('Year').AsInteger;
    aSprintRec.WeekNum := cdsSprint.FieldByName('WeekNum').AsInteger;
    aSprintRec.BeginDate := cdsSprint.FieldByName('BeginDate').AsDatetime;
    aSprintRec.EndDate := cdsSprint.FieldByName('EndDate').AsDatetime;
    aSprintRec.Capacity := cdsSprint.FieldByName('Capacity').AsInteger;
    aSprintRec.Consumed := cdsSprint.FieldByName('Consumed').AsInteger;
    aSprintRec.Planned := cdsSprint.FieldByName('Planned').AsInteger;
    result := True;
  end;
end;


function TdtmTypeList.GetTypeLabel(aClientDataset: TClientDataSet;
  aType: integer): string;
begin
  result := 'Type not found!';
  if aClientDataset.FindKey([aType]) then
    result := aClientDataset.FieldByName('Label').AsString;
end;

procedure TdtmTypeList.DataModuleCreate(Sender: TObject);
begin
  FLSNSyncDB := TLSNSyncDB.Create;
  //
  CreateLocalTypeList;
end;

procedure TdtmTypeList.DataModuleDestroy(Sender: TObject);
begin
  FLSNSyncDB.Free;
end;

procedure TdtmTypeList.Initialize(aDirectory: string);
begin
  FLSNSyncDB.RegisterDataset(cdsSprint, 'Sprints.cds');
  FLSNSyncDB.Initialize(aDirectory, True);
end;

procedure TdtmTypeList.Load;
begin
  FLSNSyncDB.Load;
end;

procedure TdtmTypeList.Save;
begin
  FLSNSyncDB.Save;
end;

end.
