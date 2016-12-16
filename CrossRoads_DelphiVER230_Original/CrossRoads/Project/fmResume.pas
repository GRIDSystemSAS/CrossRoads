unit fmResume;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.Objects,
  fmSprint, FMX.Filter.Effects, fmBurnDown, fmBurnUp, fmVelocity, fmChartNbTasks,
  FMX.Printer, IOUtils, FMX.Memo;

type
  TfrmResume = class(TForm)
    layBody: TLayout;
    layLeft: TLayout;
    imgSchedule: TImage;
    laySprint: TLayout;
    imgNext: TImage;
    MonochromeEffect4: TMonochromeEffect;
    imgPrev: TImage;
    MonochromeEffect3: TMonochromeEffect;
    layChart: TLayout;
    layBurndown: TLayout;
    rectBurndown: TRectangle;
    txtBurndown: TText;
    layBurnup: TLayout;
    rectBurnup: TRectangle;
    txtBurnup: TText;
    layOthers: TLayout;
    layVelocity: TLayout;
    rectVelocity: TRectangle;
    txtVelocity: TText;
    layNbTasks: TLayout;
    rectNbTasks: TRectangle;
    txtNbTasks: TText;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure imgPrevClick(Sender: TObject);
    procedure imgNextClick(Sender: TObject);
    procedure imgPrevMouseEnter(Sender: TObject);
    procedure imgPrevMouseLeave(Sender: TObject);
    procedure imgNextMouseEnter(Sender: TObject);
    procedure imgNextMouseLeave(Sender: TObject);
  private
    FfrmSprint: TfrmSprint;
    FCurrentDate: TDatetime;
    FfrmBurnDown: TfrmBurnDown;
    FfrmBurnUp: TfrmBurnUp;
    FfrmVelocity: TfrmVelocity;
    FfrmChartNbTasks: TfrmChartNbTasks;
    FInitOnce: Boolean;
  public
    procedure Initialize;
  end;

var
  frmResume: TfrmResume;

implementation

uses DateUtils, fmWelcomeMain, unGlobal, unStructure;

{$R *.fmx}


procedure TfrmResume.FormCreate(Sender: TObject);
begin
  FfrmSprint := TfrmSprint.Create(self);
  FfrmSprint.layBody.Parent := laySprint;
  //
  FfrmBurnDown := TfrmBurnDown.Create(self);
  FfrmBurnDown.layBody.Parent := layBurnDown;
  //
  FfrmBurnUp := TfrmBurnUp.Create(self);
  FfrmBurnUp.layBody.Parent := layBurnUp;
  //
  FfrmVelocity := TfrmVelocity.Create(self);
  FfrmVelocity.layBody.Parent := layVelocity;
  //
  FfrmChartNbTasks := TfrmChartNbTasks.Create(self);
  FfrmChartNbTasks.layBody.Parent := layNbTasks;
  //
  FInitOnce := False;
end;

procedure TfrmResume.FormDestroy(Sender: TObject);
begin
  FfrmSprint.Free;
  FfrmBurnDown.Free;
  FfrmBurnUp.Free;
  FfrmVelocity.Free;
  FfrmChartNbTasks.Free;
end;

procedure TfrmResume.imgNextClick(Sender: TObject);
begin
  FCurrentDate := IncWeek(FCurrentDate,1);
  Initialize;
end;

procedure TfrmResume.imgNextMouseEnter(Sender: TObject);
begin
  frmWelcomeMain.ShowHint('Next week...');
end;

procedure TfrmResume.imgNextMouseLeave(Sender: TObject);
begin
  frmWelcomeMain.HideHint;
end;

procedure TfrmResume.imgPrevClick(Sender: TObject);
begin
  FCurrentDate := IncWeek(FCurrentDate,-1);
  Initialize;
end;

procedure TfrmResume.imgPrevMouseEnter(Sender: TObject);
begin
  frmWelcomeMain.ShowHint('Previous week...');
end;

procedure TfrmResume.imgPrevMouseLeave(Sender: TObject);
begin
  frmWelcomeMain.HideHint;
end;

procedure TfrmResume.Initialize;
var
  lSprintRec: TSprintRecord;
begin
  if not FInitOnce then
  begin
    FInitOnce := True;
    if GlobalVar.CurrentProject.Data.LastSprint<>'' then
    begin
      TypeDatamodule.GetSprint(GlobalVar.CurrentProject.Data.LastSprint, lSprintRec);
      FCurrentDate := lSprintRec.BeginDate;
    end
    else
      FCurrentDate := date;
  end;
  //
  FfrmSprint.Initialize(FCurrentDate, CST_SprintResume);
  FfrmBurnDown.Initialize(FCurrentDate);
  FfrmBurnUp.Initialize(FCurrentDate);
  FfrmVelocity.Initialize(FCurrentDate);
  FfrmChartNbTasks.Initialize(FCurrentDate);
end;

end.
