unit dmProjectList;

interface

uses
  System.SysUtils, System.Classes, unLSNSyncDB, Data.DB, Datasnap.DBClient,
  unStructure;

type
  TdtmProjectList = class(TDataModule)
    cdsProject: TClientDataSet;
    cdsProjectProjectNum: TIntegerField;
    cdsProjectProjectName: TStringField;
    cdsProjectDescription: TStringField;
    cdsProjectCreationDate: TDateTimeField;
    cdsProjectModificationDate: TDateTimeField;
    cdsProjectColorID: TIntegerField;
    cdsProjectStatus: TIntegerField;
    cdsProjectLastTaskNum: TIntegerField;
    cdsProjectSortOrder: TIntegerField;
    cdsProjectTag: TIntegerField;
    cdsProjectCustomDate: TDateTimeField;
    cdsProjectFirstSprint: TStringField;
    cdsProjectLastSprint: TStringField;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FLSNSyncDB: TLSNSyncDB;
    //
  public
    procedure Initialize(aDirectory: string);
    procedure Load;
    procedure Save;
    procedure UpdateProject(aProjectRec: TProjectRecord);
    function GetProject(aProjectNum: integer; var aProjectRec: TProjectRecord): Boolean;

  end;

var
  dtmProjectList: TdtmProjectList;

implementation

uses unGlobal, dmTypeList;

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

procedure TdtmProjectList.DataModuleCreate(Sender: TObject);
begin
  FLSNSyncDB := TLSNSyncDB.Create;
end;

procedure TdtmProjectList.DataModuleDestroy(Sender: TObject);
begin
  FLSNSyncDB.Destroy;
end;

procedure TdtmProjectList.Initialize(aDirectory: string);
begin
  FLSNSyncDB.RegisterDataset(cdsProject, 'Projects.cds');
  FLSNSyncDB.Initialize(aDirectory, True);
end;

procedure TdtmProjectList.Load;
begin
  FLSNSyncDB.Load;
end;

procedure TdtmProjectList.Save;
begin
  FLSNSyncDB.Save;
end;

procedure TdtmProjectList.UpdateProject(aProjectRec: TProjectRecord);
begin
  if cdsProject.FindKey([aProjectRec.ProjectNum]) then
  begin
    cdsProject.Edit;
    cdsProject.FieldByName('ProjectName').AsString := aProjectRec.ProjectName;
    cdsProject.FieldByName('Description').AsString := aProjectRec.Description;
    cdsProject.FieldByName('ModificationDate').AsDateTime := aProjectRec.ModificationDate;
    cdsProject.FieldByName('ColorID').AsInteger := aProjectRec.ColorID;
    cdsProject.FieldByName('Status').AsInteger := aProjectRec.Status;
    cdsProject.FieldByName('LastTaskNum').AsInteger := aProjectRec.LastTaskNum;
    cdsProject.FieldByName('SortOrder').AsInteger := aProjectRec.SortOrder;
    cdsProject.FieldByName('Tag').AsInteger := aProjectRec.Tag;
    cdsProject.FieldByName('CustomDate').AsDateTime := aProjectRec.CustomDate;
    cdsProject.FieldByName('FirstSprint').AsString := aProjectRec.FirstSprint;
    cdsProject.FieldByName('LastSprint').AsString := aProjectRec.LastSprint;
    cdsProject.Post;
  end
  else
  begin
    cdsProject.Insert;
    cdsProject.FieldByName('ProjectNum').AsInteger := aProjectRec.ProjectNum;
    cdsProject.FieldByName('ProjectName').AsString := aProjectRec.ProjectName;
    cdsProject.FieldByName('Description').AsString := aProjectRec.Description;
    cdsProject.FieldByName('CreationDate').AsDateTime := aProjectRec.CreationDate;
    cdsProject.FieldByName('ModificationDate').AsDateTime := aProjectRec.ModificationDate;
    cdsProject.FieldByName('ColorID').AsInteger := aProjectRec.ColorID;
    cdsProject.FieldByName('Status').AsInteger := aProjectRec.Status;
    cdsProject.FieldByName('LastTaskNum').AsInteger := aProjectRec.LastTaskNum;
    cdsProject.FieldByName('SortOrder').AsInteger := aProjectRec.SortOrder;
    cdsProject.FieldByName('Tag').AsInteger := aProjectRec.Tag;
    cdsProject.FieldByName('CustomDate').AsDateTime := aProjectRec.CustomDate;
    cdsProject.FieldByName('FirstSprint').AsString := aProjectRec.FirstSprint;
    cdsProject.FieldByName('LastSprint').AsString := aProjectRec.LastSprint;
    cdsProject.Post;
  end;
end;

function TdtmProjectList.GetProject(aProjectNum: integer; var aProjectRec: TProjectRecord): Boolean;
begin
  result := False;
  if cdsProject.FindKey([aProjectNum]) then
  begin
    aProjectRec.ProjectNum := cdsProject.FieldByName('ProjectNum').AsInteger;
    aProjectRec.ProjectName := cdsProject.FieldByName('ProjectName').AsString;
    aProjectRec.Description := cdsProject.FieldByName('Description').AsString;
    aProjectRec.CreationDate := cdsProject.FieldByName('CreationDate').AsDateTime;
    aProjectRec.ModificationDate := cdsProject.FieldByName('ModificationDate').AsDateTime;
    aProjectRec.ColorID := cdsProject.FieldByName('ColorID').AsInteger;
    aProjectRec.Status := cdsProject.FieldByName('Status').AsInteger;
    aProjectRec.LastTaskNum := cdsProject.FieldByName('LastTaskNum').AsInteger;
    aProjectRec.SortOrder := cdsProject.FieldByName('SortOrder').AsInteger;
    aProjectRec.Tag := cdsProject.FieldByName('Tag').AsInteger;
    aProjectRec.CustomDate := cdsProject.FieldByName('CustomDate').AsDateTime;
    aProjectRec.FirstSprint := cdsProject.FieldByName('FirstSprint').AsString;
    aProjectRec.LastSprint := cdsProject.FieldByName('LastSprint').AsString;
    result := True;
  end;
end;


end.
