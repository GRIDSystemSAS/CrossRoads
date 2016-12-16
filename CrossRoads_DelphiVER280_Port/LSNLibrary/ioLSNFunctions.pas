unit ioLSNFunctions;

interface

uses
  SysUtils, Types, UITypes, Classes, Variants, FMX_Types, FMX_Controls, FMX_Forms,
  FMX_Dialogs, FMX_Objects, FMX_Platform;

type
  TLSNPlatform = class
  private
    FHomeDirectory, FDocumentsDirectory, FLibraryDirectory, FTempDirectory: string;
    FDataDirectory, FArchiveDirectory: string;
    FWidth, FHeight: extended;
    FLanguage: string;
    //
    function GetWidth: extended;
    function GetHeight: extended;
  public
    constructor Create;
    destructor Destroy; override;
    //
    property Width: extended read GetWidth;
    property Height: extended read FHeight;
    property HomeDirectory: string read FHomeDirectory;
    property DocumentsDirectory: string read FDocumentsDirectory;
    property LibraryDirectory: string read FLibraryDirectory;
    property TempDirectory: string read FTempDirectory;
    property DataDirectory: string read FDataDirectory;
    property ArchiveDirectory: string read FArchiveDirectory;
    property Language: string read FLanguage;
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


implementation

uses StrUtils;

const
  CST_True = 'True';
  CST_False = 'False';


{$REGION 'TLSNPath'}

constructor TLSNPlatform.Create;
var
  lAppli, lDir: string;
begin
  FLanguage := UpperCase(Platform.GetCurrentLangID);
  //
  lAppli := ParamStr(0);
  FHomeDirectory := ExtractFilePath(lAppli);
  {$IFDEF FPC}
    FDocumentsDirectory := FHomeDirectory+'../Documents/';
    FLibraryDirectory := FHomeDirectory+'../Library/';
    FTempDirectory := FHomeDirectory+'../tmp/';
    //
    if DirectoryExists(FLibraryDirectory) then
    begin
      FDataDirectory := FLibraryDirectory+'/Data/';
      if not DirectoryExists(FDataDirectory) then
        CreateDir(FDataDirectory);
      //FDataDirectory := FDataDirectory+'/';
    end
    else
      FDataDirectory := '#ERROR';
    //
    if DirectoryExists(FDocumentsDirectory) then
    begin
      FArchiveDirectory := FDocumentsDirectory+'/Archive/';
      if not DirectoryExists(FArchiveDirectory) then
        CreateDir(FArchiveDirectory);
    end
    else
      FArchiveDirectory := '#ERROR';
  {$ELSE}
    lDir := FHomeDirectory+'Data\';
    if not DirectoryExists(lDir) then
      CreateDir(lDir);
    FDocumentsDirectory := lDir+'Import\';
    if not DirectoryExists(FDocumentsDirectory) then
      CreateDir(FDocumentsDirectory);
    FArchiveDirectory := lDir+'Archive\';
    if not DirectoryExists(FArchiveDirectory) then
      CreateDir(FArchiveDirectory);
    FLibraryDirectory := lDir;
    FTempDirectory := lDir;
    FDataDirectory := lDir;
  {$ENDIF}
end;

destructor TLSNPlatform.Destroy;
begin

  inherited;
end;

function TLSNPlatform.GetHeight: extended;
var
  lPointF: TPointF;
begin
  lPointF := Platform.GetScreenSize;
  result := lPointF.Y;
end;

function TLSNPlatform.GetWidth: extended;
var
  lPointF: TPointF;
begin
  lPointF := Platform.GetScreenSize;
  result := lPointF.X;
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
  if FileExists(FFilename) then
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

end.
