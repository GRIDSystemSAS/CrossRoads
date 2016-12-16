//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
// unLSNUtils v.1.0
// by Laurent Sengmany - 30.11.2011
//-----------------------------------------------------------------------------
// Description : this unit contains several classes and functions:
//               - TLSNPath: paths settings
//
// How-to TLSNPath:
//      // create paths:
//      FMyPath := TLSNPath.Create('DBTest');
//      // created paths:
//      FMyPath.ApplicationDirectory, FMyPath.DataDirectory, FMyPath.ParameterDirectory
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
unit unLSNUtils;

interface

uses
  System.SysUtils, System.Classes, IOUtils, System.Types;

type
  TLSNPath = class
  private
    FApplicationDirectory, FDataDirectory: string;
    FParameterDirectory, FLogDirectory: string;
  public
    constructor Create(aApplicationName: string);
    destructor Destroy; override;
    //
    property ApplicationDirectory: string read FApplicationDirectory;
    property DataDirectory: string read FDataDirectory;
    property ParameterDirectory: string read FParameterDirectory;
    property LogDirectory: string read FLogDirectory;
  end;

  TLSNParamElement = class
  private
    FParamCode: string;
    FParamValue: string;
  public
    constructor Create(aLine: string);
    destructor Destroy; override;
    //
    property ParamCode: string read FParamCode write FParamCode;
    property ParamValue: string read FParamValue write FParamValue;
  end;

  TLSNParameters = class
  private
    FFilename: string;
    FFile: TStringList;
    FParams: TList;
    //
    procedure AddParam(aLine: string);
    function GetParam(aParamCode: string): TLSNParamElement;
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
    constructor Create(aFilename: string);
    destructor Destroy; override;
    procedure Save;
    //
    property ParamString[aParamName: string]: string read GetParamString write SetParamString;
    property ParamInteger[aParamName: string]: Integer read GetParamInteger write SetParamInteger;
    property ParamFloat[aParamName: string]: extended read GetParamFloat write SetParamFloat;
    property ParamBoolean[aParamName: string]: Boolean read GetParamBoolean write SetParamBoolean;
    property ParamDatetime[aParamName: string]: TDatetime read GetParamDatetime write SetParamDatetime;

  end;

  TLogType = (ltInformation, ltWarning, ltError, ltDebug);

  TLSNLog = class
  private
    FDirectory, FFilename, FFileType: string;
    FFile: TStringList;
    FShowDebug: Boolean;
    FDaysBeforeDelete: integer;
    FBeginDate: TDatetime;
    //
    procedure WriteToLog(aLogType: TLogType; aMessage: string);
    function LogTypeToText(aLogType: TLogType): string;
    procedure PurgeLogFiles;
  public
    constructor Create(aDirectory, aFileType: string; aDaysBeforeDelete: Integer= 1);
    destructor Destroy; override;
    //
    procedure WriteInfo(aMessage: string);
    procedure WriteWarning(aMessage: string);
    procedure WriteError(aMessage: string);
    procedure WriteDebug(aMessage: string);
    //
    property ShowDebug: Boolean read FShowDebug write FShowDebug;
    //property DaysBeforeDelete: integer read FDaysBeforeDelete write FDaysBeforeDelete;
  end;

  function LSNCreateUniqueID: string;

  function LSNFormatDatetime(aFormat: string; aDate: TDatetime): string;
  function LSNFormatFixedText(aText: string; aSize: integer): string;

implementation

uses StrUtils;

const
  CST_True = 'True';
  CST_False = 'False';
  //
  CST_logSeparator = '-------------------------------------------------------------------------------------------------------------';

{$REGION 'TLSNPath'}

constructor TLSNPath.Create(aApplicationName: string);
begin
  FApplicationDirectory := TPath.GetHomePath+TPath.DirectorySeparatorChar+aApplicationName;
  if not TDirectory.Exists(FApplicationDirectory) then
    TDirectory.CreateDirectory(FApplicationDirectory);
  //
  FDataDirectory := FApplicationDirectory+TPath.DirectorySeparatorChar+'Data';
  if not TDirectory.Exists(FDataDirectory) then
    TDirectory.CreateDirectory(FDataDirectory);
  //
  FParameterDirectory := FApplicationDirectory+TPath.DirectorySeparatorChar+'Param';
  if not TDirectory.Exists(FParameterDirectory) then
    TDirectory.CreateDirectory(FParameterDirectory);
  //
  FLogDirectory := FApplicationDirectory+TPath.DirectorySeparatorChar+'Log';
  if not TDirectory.Exists(FLogDirectory) then
    TDirectory.CreateDirectory(FLogDirectory);
end;

destructor TLSNPath.Destroy;
begin
  //
end;

{$ENDREGION}

{$REGION 'TLSNParameters'}


constructor TLSNParamElement.Create(aLine: string);
var
  lTokenPos, lLen: integer;
begin
  lTokenPos := Pos('=',aLine);
  lLen := Length(aLine);
  FParamCode := LeftStr(aLine,lTokenPos-1);
  FParamValue := RightStr(aLine,lLen-lTokenPos);
end;

destructor TLSNParamElement.Destroy;
begin

end;



constructor TLSNParameters.Create(aFilename: string);
var
  i: integer;
begin
  FFilename := aFilename;
  FFile := TStringList.Create;
  FParams := TList.Create;
  // load param from existing file...
  if TFile.Exists(FFilename) then
  begin
    FFile.LoadFromFile(FFilename);
    for i := 0 to FFile.Count-1 do
      AddParam(FFile[i]);
  end;
end;


destructor TLSNParameters.Destroy;
begin
  FFile.Free;
  FParams.Free;
end;

procedure TLSNParameters.AddParam(aLine: string);
var
  lToken: TStringList;
  lParam: TLSNParamElement;
begin
  lParam := TLSNParamElement.Create(aLine);
  FParams.Add(lParam);
end;

procedure TLSNParameters.Save;
var
  i: integer;
  lParam: TLSNParamElement;
begin
  FFile.Clear;
  for i := 0 to FParams.Count-1 do
  begin
    lParam := FParams[i];
    FFile.Add(Format('%s=%s',[lParam.ParamCode, lParam.ParamValue]));
  end;
  FFile.SaveToFile(FFilename);
end;

function TLSNParameters.GetParam(aParamCode: string): TLSNParamElement;
var
  i: integer;
  lParam: TLSNParamElement;
  lLine: string;
begin
  result := nil;
  for i := 0 to FParams.Count-1 do
  begin
    lParam := FParams[i];
    if lParam.ParamCode=aParamCode then
    begin
      result := lParam;
      exit;
    end;
  end;
  // create new param if not found
  if result=nil then
  begin
    lLine := Format('%s=%s',[aParamCode, '']);
    lParam := TLSNParamElement.Create(lLine);
    FParams.Add(lParam);
    result := lParam;
  end;
end;

procedure TLSNParameters.SetParamString(aParamName, aValueText: string);
var
  lParam: TLSNParamElement;
begin
  lParam := GetParam(aParamName);
  lParam.ParamValue := aValueText;
end;

procedure TLSNParameters.SetParamInteger(aParamName: string; aValueInteger: integer);
var
  lParam: TLSNParamElement;
begin
  lParam := GetParam(aParamName);
  lParam.ParamValue := IntToStr(aValueInteger);
end;

procedure TLSNParameters.SetParamFloat(aParamName: string; aValueFloat: extended);
var
  lParam: TLSNParamElement;
begin
  lParam := GetParam(aParamName);
  lParam.ParamValue := FloatToStr(aValueFloat);
end;

procedure TLSNParameters.SetParamBoolean(aParamName: string; aValueBoolean: Boolean);
var
  lParam: TLSNParamElement;
begin
  lParam := GetParam(aParamName);
  if aValueBoolean then
    lParam.ParamValue := CST_True
  else
    lParam.ParamValue := CST_False;
end;

procedure TLSNParameters.SetParamDatetime(aParamName: string; aValueDatetime: TDatetime);
var
  lParam: TLSNParamElement;
begin
  lParam := GetParam(aParamName);
  lParam.ParamValue := DateToStr(aValueDatetime);
end;

function TLSNParameters.GetParamString(aParamName: string): string;
var
  lParam: TLSNParamElement;
begin
  lParam := GetParam(aParamName);
  result := lParam.ParamValue;
end;

function TLSNParameters.GetParamInteger(aParamName: string): Integer;
var
  lParam: TLSNParamElement;
begin
  lParam := GetParam(aParamName);
  if lParam.ParamValue='' then
    result := 0
  else
    result := StrToInt(lParam.ParamValue);
end;

function TLSNParameters.GetParamFloat(aParamName: string): extended;
var
  lParam: TLSNParamElement;
begin
  lParam := GetParam(aParamName);
  if lParam.ParamValue='' then
    result := 0.0
  else
    result := StrToFloat(lParam.ParamValue);
end;

function TLSNParameters.GetParamBoolean(aParamName: string): Boolean;
var
  lParam: TLSNParamElement;
begin
  lParam := GetParam(aParamName);
  if lParam.ParamValue=CST_True then
    result := True
  else
    result := False;
end;

function TLSNParameters.GetParamDatetime(aParamName: string): TDatetime;
var
  lParam: TLSNParamElement;
begin
  lParam := GetParam(aParamName);
  if lParam.ParamValue='' then
    result := 0
  else
    result := StrToDate(lParam.ParamValue);
end;

{$ENDREGION}

{$REGION 'TLSNLog'}

constructor TLSNLog.Create(aDirectory, aFileType: string; aDaysBeforeDelete: Integer= 1);
begin
  FShowDebug := False;
  FDaysBeforeDelete := aDaysBeforeDelete;
  //
  FDirectory := aDirectory;
  FFileType := aFileType;
  FFilename := FDirectory + TPath.DirectorySeparatorChar
               + Format('%s_%s.log',[FFileType, FormatDatetime('YYYYMMDDhhnnss', Now)]);
  FFile := TStringList.Create;
  //
  FFile.Add(CST_logSeparator);
  FFile.Add(Format('-- %s - %s',[FFileType, FormatDatetime('DD.MM.YYYY', Date)]));
  FFile.Add(Format('-- Days before delete: %d.',[FDaysBeforeDelete]));
  FFile.Add(CST_logSeparator);
  FFile.Add(CST_logSeparator);
  FBeginDate := Now;
  FFile.Add(Format('=> Job started on %s.',[FormatDatetime('DD.MM.YYYY hh:nn:ss', FBeginDate)]));
  FFile.Add(CST_logSeparator);
  FFile.Add('');
  FFile.SaveToFile(FFilename);
  //

end;

destructor TLSNLog.Destroy;
var
  lEndDate, lDuration: TDatetime;
begin
  PurgeLogFiles;
  //
  lEndDate := Now;
  lDuration := lEndDate-FBeginDate;
  FFile.Add('');
  FFile.Add(CST_logSeparator);
  FFile.Add(Format('=> Job ended on %s.',[FormatDatetime('DD.MM.YYYY hh:nn:ss', lEndDate)]));
  FFile.Add(Format('=> Duration: %s.',[FormatDatetime('hh:nn:ss', lDuration)]));
  FFile.Add(CST_logSeparator);
  FFile.SaveToFile(FFilename);
end;

procedure TLSNLog.WriteToLog(aLogType: TLogType; aMessage: string);
var
  lText: string;
begin
  lText := FormatDatetime('hh:nn:ss', Now);
  lText := lText + ' ' + LogTypeToText(aLogType);
  lText := lText + ' ' + aMessage;
  FFile.Add(lText);
  FFile.SaveToFile(FFilename);
end;

function TLSNLog.LogTypeToText(aLogType: TLogType): string;
var
  lText: string;
begin
  case aLogType of
    ltInformation: lText := '[INFO]';
    ltWarning: lText := '[WARNING]';
    ltError: lText := '[ERROR]';
    ltDebug: lText := '[DEBUG]';
  end;
  result := LSNFormatFixedText(lText, 12);
end;

procedure TLSNLog.WriteInfo(aMessage: string);
begin
  WriteToLog(ltInformation, aMessage);
end;

procedure TLSNLog.WriteWarning(aMessage: string);
begin
  WriteToLog(ltWarning, aMessage);
end;

procedure TLSNLog.WriteError(aMessage: string);
begin
  WriteToLog(ltError, aMessage);
end;

procedure TLSNLog.WriteDebug(aMessage: string);
begin
  if FShowDebug then
    WriteToLog(ltDebug, aMessage);
end;

procedure TLSNLog.PurgeLogFiles;
var
  lFilename: string;
  i: integer;
  lFiles: TStringDynArray;
begin
  lFiles := TDirectory.GetFiles(FDirectory);
  for i := 0 to Length(lFiles)-1 do
  begin
    lFilename := lFiles[i];
    if TFile.GetCreationTime(lFilename)<Date-FDaysBeforeDelete-1 then
      TFile.Delete(lFilename);
  end;
end;


{$ENDREGION}


function LSNCreateUniqueID: string;
var
  lStrID: string;
begin
  lStrID := RightStr('0000'+IntToStr(Random(9999)),4);
  lStrID := lStrID + '-' + FormatDatetime('YYYYMMDDhhnnss', Now);
  result := lStrID;
end;

function LSNFormatDatetime(aFormat: string; aDate: TDatetime): string;
begin
  try
    result := FormatDatetime(aFormat, aDate);
  except
    result := FormatDatetime(aFormat, 0);
  end;
end;

function LSNFormatFixedText(aText: string; aSize: integer): string;
var
  lTmp: string;
  lLen, i: integer;
begin
  lLen := Length(aText);
  if (lLen>aSize) then
    result := LeftStr(aText, aSize)
  else
  begin
    lTmp := aText;
    for i := 1 to aSize-lLen do
      lTmp := lTmp + ' ';
    result := lTmp;
  end;
end;

end.
