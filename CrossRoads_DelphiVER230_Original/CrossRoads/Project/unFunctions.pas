unit unFunctions;

interface

uses System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Ani,
  FMX.Menus, FMX.Layouts;

//type


function ProjectNumToColor(aProjectNum: integer): Cardinal;
function PriorityToColor(aPriority: integer): Cardinal;
// Sprint
function GetSprintCode(aDate: TDatetime): string;
function SprintCodeToInt(aSprintCode: string): integer;
//
function DateToChartDate(aDate: Tdatetime): string;
// compare functions
function CompareDoneDate(Item1, Item2: Pointer): Integer;

implementation

uses DateUtils, StrUtils, dmTypeList, unGlobal;



function ProjectNumToColor(aProjectNum: integer): Cardinal;
var
  lMod: integer;
begin
  lMod := aProjectNum mod 9;
  case lMod of
    1: result := TAlphaColorRec.Lightcyan;
    2: result := TAlphaColorRec.Lemonchiffon;
    3: result := TAlphaColorRec.Lightpink;
    4: result := TAlphaColorRec.Palegreen;
    5: result := TAlphaColorRec.Plum;
    6: result := TAlphaColorRec.Lightskyblue;
    7: result := TAlphaColorRec.Khaki;
    8: result := TAlphaColorRec.Greenyellow;
    else result := TAlphaColorRec.Lightsteelblue;
  end;
end;

function PriorityToColor(aPriority: integer): Cardinal;
begin
  case aPriority of
    priNotDefined : result := TAlphaColorRec.Whitesmoke;
    priVeryLow : result := TAlphaColorRec.Lightgreen;
    priLow : result := TAlphaColorRec.Limegreen;
    priMedium : result := TAlphaColorRec.Yellow;
    priHigh : result := TAlphaColorRec.Orange;
    priVeryHigh : result := TAlphaColorRec.Red;
    priCritical : result := TAlphaColorRec.Maroon;
    else result := TAlphaColorRec.Whitesmoke;
  end;
end;

function GetSprintCode(aDate: TDatetime): string;
var
  lYear, lWeek: Word;
begin
  lWeek := WeekOfTheYear(aDate, lYear);
  result := IntToStr(lYear)+'-'+RightStr('0'+IntToStr(lWeek),2);
end;

function SprintCodeToInt(aSprintCode: string): integer;
begin
  result := StrToInt(StringReplace(aSprintCode, '-', '', [rfReplaceAll]));
end;

function DateToChartDate(aDate: Tdatetime): string;
begin
  result := FormatDatetime(CST_DateForChart, aDate);
end;

function CompareDoneDate(Item1, Item2: Pointer): Integer;
var
  lTask1, lTask2: TTask;
begin
  lTask1 := Item1;
  lTask2 := Item2;
  Result := CompareDate(lTask1.Data.DoneDate,lTask2.Data.DoneDate);
end;

end.
