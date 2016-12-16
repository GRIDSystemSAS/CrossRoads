unit fmSprintSmall;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Layouts,
  unStructure, unGlobal;

type
  TfrmSprintSmall = class(TForm)
    layBody: TLayout;
    rectCalBody: TRectangle;
    rectHeader: TRectangle;
    layDate: TLayout;
    txtYear: TText;
    txtWeekNum: TText;
    txtPeriod: TText;
    layContent: TLayout;
  private
    FSprintRec: TSprintRecord;
    FProject: TProject;

  public
    procedure Initialize(aDate: TDatetime);
  end;

var
  frmSprintSmall: TfrmSprintSmall;

implementation

uses DateUtils, StrUtils, fmTaskPostIt;

{$R *.fmx}

procedure TfrmSprintSmall.Initialize(aDate: TDatetime);
var
  lID: string;
  i: integer;
const
  CST_DateMonth = 'DD.MM';
begin
  lID := IntToStr(YearOf(aDate))+'-'+RightStr('0'+IntToStr(WeekOf(aDate)),2);
  TypeDatamodule.GetSprint(lID, FSprintRec);
  txtYear.Text := IntToSTr(FSprintRec.Year);
  txtWeekNum.Text := IntToSTr(FSprintRec.WeekNum);
  txtPeriod.Text := Format('%s-%s',[FormatDatetime(CST_DateMonth,FSprintRec.BeginDate),
                                            FormatDatetime(CST_DateMonth,FSprintRec.EndDate)]);
end;


end.
