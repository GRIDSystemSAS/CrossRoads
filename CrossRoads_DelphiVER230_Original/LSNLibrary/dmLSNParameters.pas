unit dmLSNParameters;

interface

uses
  System.SysUtils, System.Classes, unLSNSyncDB, Data.DB, Datasnap.DBClient, IOUtils,
  Inifiles;

type
  TdtmLSNParameters = class(TDataModule)
    cdsParameters: TClientDataSet;
    cdsParametersParamName: TStringField;
    cdsParametersValueText: TStringField;
    cdsParametersValueInteger: TIntegerField;
    cdsParametersValueFloat: TFloatField;
    cdsParametersValueBoolean: TBooleanField;
    cdsParametersValueDatetime: TDateTimeField;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    FLSNSyncDB: TLSNSyncDB;
    //
    procedure SetParamString(aParamName, aValueText: string);
    function GetParamString(aParamName: string): string;
    procedure SetParamInteger(aParamName: string; aValueInteger: integer);
    function GetParamInteger(aParamName: string): Integer;
    procedure SetParamFloat(aParamName: string; aValueFloat: extended);
    function GetParamFloat(aParamName: string): extended;
    procedure SetParamBoolean(aParamName: string; aValueBoolean: Boolean);
    function GetParamBoolean(aParamName: string): Boolean;
    procedure SetParamDatetime(aParamName: string; aValueDatetime: TDatetime);
    function GetParamDatetime(aParamName: string): TDatetime;
  public
    procedure Initialize(aDirectory, aSaveName: string);
    procedure Load;
    procedure Save;
    // Parameters properties
    property ParamString[aParamName: string]: string read GetParamString write SetParamString;
    property ParamInteger[aParamName: string]: Integer read GetParamInteger write SetParamInteger;
    property ParamFloat[aParamName: string]: extended read GetParamFloat write SetParamFloat;
    property ParamBoolean[aParamName: string]: Boolean read GetParamBoolean write SetParamBoolean;
    property ParamDatetime[aParamName: string]: TDatetime read GetParamDatetime write SetParamDatetime;
  end;

var
  dtmLSNParameters: TdtmLSNParameters;


implementation

const
  CST_Common = 'Common';

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

procedure TdtmLSNParameters.DataModuleCreate(Sender: TObject);
begin
  FLSNSyncDB := TLSNSyncDB.Create;

end;

procedure TdtmLSNParameters.DataModuleDestroy(Sender: TObject);
begin
  FLSNSyncDB.Destroy;
end;

procedure TdtmLSNParameters.Initialize(aDirectory, aSaveName: string);
begin
  FLSNSyncDB.RegisterDataset(cdsParameters, aSaveName);
  FLSNSyncDB.Initialize(aDirectory, True);
end;

procedure TdtmLSNParameters.Load;
begin
  FLSNSyncDB.Load;
end;

procedure TdtmLSNParameters.Save;
begin
  FLSNSyncDB.Save;
end;

{$REGION 'SetParam & GetParam'}

procedure TdtmLSNParameters.SetParamString(aParamName, aValueText: string);
begin
  if cdsParameters.FindKey([aParamName]) then
  begin
    cdsParameters.Edit;
    cdsParameters.FieldByName('ValueText').AsString := aValueText;
    cdsParameters.Post;
  end
  else
  begin
    cdsParameters.Insert;
    cdsParameters.FieldByName('ParamName').AsString := aParamName;
    cdsParameters.FieldByName('ValueText').AsString := aValueText;
    cdsParameters.Post;
  end;
end;

procedure TdtmLSNParameters.SetParamInteger(aParamName: string; aValueInteger: integer);
begin
  if cdsParameters.FindKey([aParamName]) then
  begin
    cdsParameters.Edit;
    cdsParameters.FieldByName('ValueInteger').AsInteger := aValueInteger;
    cdsParameters.Post;
  end
  else
  begin
    cdsParameters.Insert;
    cdsParameters.FieldByName('ParamName').AsString := aParamName;
    cdsParameters.FieldByName('ValueInteger').AsInteger := aValueInteger;
    cdsParameters.Post;
  end;
end;

procedure TdtmLSNParameters.SetParamFloat(aParamName: string; aValueFloat: extended);
begin
  if cdsParameters.FindKey([aParamName]) then
  begin
    cdsParameters.Edit;
    cdsParameters.FieldByName('ValueFloat').AsFloat := aValueFloat;
    cdsParameters.Post;
  end
  else
  begin
    cdsParameters.Insert;
    cdsParameters.FieldByName('ParamName').AsString := aParamName;
    cdsParameters.FieldByName('ValueFloat').AsFloat := aValueFloat;
    cdsParameters.Post;
  end;
end;

procedure TdtmLSNParameters.SetParamBoolean(aParamName: string; aValueBoolean: Boolean);
begin
  if cdsParameters.FindKey([aParamName]) then
  begin
    cdsParameters.Edit;
    cdsParameters.FieldByName('ValueBoolean').AsBoolean := aValueBoolean;
    cdsParameters.Post;
  end
  else
  begin
    cdsParameters.Insert;
    cdsParameters.FieldByName('ParamName').AsString := aParamName;
    cdsParameters.FieldByName('ValueBoolean').AsBoolean := aValueBoolean;
    cdsParameters.Post;
  end;
end;

procedure TdtmLSNParameters.SetParamDatetime(aParamName: string; aValueDatetime: TDatetime);
begin
  if cdsParameters.FindKey([aParamName]) then
  begin
    cdsParameters.Edit;
    cdsParameters.FieldByName('ValueDatetime').AsDatetime := aValueDatetime;
    cdsParameters.Post;
  end
  else
  begin
    cdsParameters.Insert;
    cdsParameters.FieldByName('ParamName').AsString := aParamName;
    cdsParameters.FieldByName('ValueDatetime').AsDatetime := aValueDatetime;
    cdsParameters.Post;
  end;
end;

function TdtmLSNParameters.GetParamString(aParamName: string): string;
begin
  if cdsParameters.FindKey([aParamName]) then
    result := cdsParameters.FieldByName('ValueText').AsString
  else
    result := '';
end;

function TdtmLSNParameters.GetParamInteger(aParamName: string): Integer;
begin
  if cdsParameters.FindKey([aParamName]) then
    result := cdsParameters.FieldByName('ValueInteger').AsInteger
  else
    result := 0;
end;

function TdtmLSNParameters.GetParamFloat(aParamName: string): extended;
begin
  if cdsParameters.FindKey([aParamName]) then
    result := cdsParameters.FieldByName('ValueFloat').AsFloat
  else
    result := 0.0;
end;

function TdtmLSNParameters.GetParamBoolean(aParamName: string): Boolean;
begin
  if cdsParameters.FindKey([aParamName]) then
    result := cdsParameters.FieldByName('ValueBoolean').AsBoolean
  else
    result := False;
end;

function TdtmLSNParameters.GetParamDatetime(aParamName: string): TDatetime;
begin
  if cdsParameters.FindKey([aParamName]) then
    result := cdsParameters.FieldByName('ValueDatetime').AsDatetime
  else
    result := 0;
end;

{$ENDREGION}





end.
