unit fmSprintCal;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Layouts,
  unStructure;

type
  TfrmSprintCal = class(TForm)
    layBody: TLayout;
    imgCalendar: TImage;
    txtYear: TText;
    txtWeekNum: TText;
    rectDateBoday: TRectangle;
    txtPeriod: TText;
  private
    FSprintRec: TSprintRecord;
  public
    procedure Initialize(aDate: TDatetime);
  end;

var
  frmSprintCal: TfrmSprintCal;

implementation

uses DateUtils, StrUtils, fmTaskPostIt, unGlobal, unFunctions;

{$R *.fmx}

procedure TfrmSprintCal.Initialize(aDate: TDatetime);
var
  lID: string;
begin
  lID := GetSprintCode(aDate);
  TypeDatamodule.GetSprint(lID, FSprintRec);
  txtYear.Text := IntToSTr(FSprintRec.Year);
  txtWeekNum.Text := IntToSTr(FSprintRec.WeekNum);
  txtPeriod.Text := Format('%s-%s',[FormatDatetime(CST_DateMonth,FSprintRec.BeginDate),
                                            FormatDatetime(CST_DateMonth,FSprintRec.EndDate)]);
end;

end.
