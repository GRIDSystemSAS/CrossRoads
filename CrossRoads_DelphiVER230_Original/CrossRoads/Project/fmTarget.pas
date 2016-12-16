unit fmTarget;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, unStructure, FMX.Objects,
  FMX.Layouts, FMX.Effects, FMX.Ani, FMX.Filter.Effects;

type
  TfrmTarget = class(TForm)
    layBody: TLayout;
    layVisible: TLayout;
    rectBody: TRectangle;
    txtDescription: TText;
    imgTarget: TImage;
    imgEdit: TImage;
    FloatAnimation1: TFloatAnimation;
    MonochromeEffect1: TMonochromeEffect;
    layDescLeft: TLayout;
    txtProgress1: TText;
    txtProgress2: TText;
    imgDelete: TImage;
    FloatAnimation2: TFloatAnimation;
    MonochromeEffect2: TMonochromeEffect;
    imgArchive: TImage;
    FloatAnimation3: TFloatAnimation;
    MonochromeEffect3: TMonochromeEffect;
    procedure txtDescriptionMouseEnter(Sender: TObject);
    procedure txtDescriptionMouseLeave(Sender: TObject);
    procedure imgEditClick(Sender: TObject);
    procedure txtDescriptionClick(Sender: TObject);
    procedure imgEditMouseEnter(Sender: TObject);
    procedure imgEditMouseLeave(Sender: TObject);
    procedure imgDeleteMouseEnter(Sender: TObject);
    procedure imgDeleteMouseLeave(Sender: TObject);
    procedure imgArchiveMouseEnter(Sender: TObject);
    procedure imgArchiveMouseLeave(Sender: TObject);
  private
    FTaskRecord: TTaskRecord;
    FType, FTaskNum: integer;
    FProjectColor: Cardinal;
    FSelected: Boolean;
    procedure SetOnDeleteClick(aEvent: TNotifyEvent);
    procedure SetOnArchiveClick(aEvent: TNotifyEvent);
  public
    procedure Initialize(aType: integer); overload;
    procedure Initialize(aTaskRec: TTaskRecord; aDelay: single= 0); overload;
    procedure RefreshData(aDelay: single);
    procedure SelectTarget;
    procedure UnSelectTarget;
    //
    property Selected: Boolean read FSelected write FSelected;
    property TaskNum: integer read FTaskNum write FTaskNum;
    property OnDeleteClick: TNotifyEvent write SetOnDeleteClick;
    property OnArchiveClick: TNotifyEvent write SetOnArchiveClick;
  end;

var
  frmTarget: TfrmTarget;

implementation

uses unGlobal, fmTargetModify, dmTypeList, fmWelcomeMain, fmTasks;

{$R *.fmx}

procedure TfrmTarget.Initialize(aType: integer);
begin
  layVisible.Visible := False;
  layVisible.Opacity := 0;
  txtDescription.Text := '';
  FType := aType;
  rectBody.Stroke.Color := TAlphaColorRec.Silver;
  //
  UnSelectTarget;
end;

procedure TfrmTarget.imgArchiveMouseEnter(Sender: TObject);
begin
  frmWelcomeMain.ShowHint('Archive a target...');
end;

procedure TfrmTarget.imgArchiveMouseLeave(Sender: TObject);
begin
  frmWelcomeMain.HideHint;
end;

procedure TfrmTarget.imgDeleteMouseEnter(Sender: TObject);
begin
  frmWelcomeMain.ShowHint('Delete a target...');
end;

procedure TfrmTarget.imgDeleteMouseLeave(Sender: TObject);
begin
  frmWelcomeMain.HideHint;
end;

procedure TfrmTarget.imgEditClick(Sender: TObject);
var
  lTask: TTask;
begin
  lTask := GlobalVar.CurrentProject.TargetSearch(FTaskRecord.TaskNum);
  if frmTargetModify.Show(FTaskRecord, FType) then
  begin
    lTask.RefreshData;
    FTaskRecord := lTask.Data;
    layVisible.Opacity := 0;
    RefreshData(0);
  end;
end;

procedure TfrmTarget.imgEditMouseEnter(Sender: TObject);
begin
  frmWelcomeMain.ShowHint('Modify a target...');
end;

procedure TfrmTarget.imgEditMouseLeave(Sender: TObject);
begin
  frmWelcomeMain.HideHint;
end;

procedure TfrmTarget.Initialize(aTaskRec: TTaskRecord; aDelay: single);
var
  lProject: TProject;
begin
  FTaskRecord := aTaskRec;
  FTaskNum := FTaskRecord.TaskNum;
  layVisible.Visible := True;
  txtDescription.Tag := FTaskRecord.TaskNum;
  imgDelete.Tag := FTaskRecord.TaskNum;
  imgArchive.Tag := FTaskRecord.TaskNum;
  //
  RefreshData(aDelay);
  //
  lProject := GlobalVar.ProjectList.Search(FTaskRecord.ProjectNum);
  FProjectColor := lProject.Data.ColorID;
end;

procedure TfrmTarget.RefreshData(aDelay: single);
var
  lDone, lChildren: integer;
  lPercent: integer;
begin
  txtDescription.Text := FTaskRecord.TaskName;
  //
  lDone := TaskDatamodule.GetDoneForTargetCount(FTaskRecord.ProjectNum, FTaskNum);
  lChildren := TaskDatamodule.GetToDoForTargetCount(FTaskRecord.ProjectNum, FTaskNum);
  txtProgress2.Text := Format('%d/%d',[ lDone, lChildren]);
  //
  if (lChildren=0) then
    lPercent := 0
  else
    lPercent := round(lDone/lChildren*100);
  txtProgress1.Text := Format('%d',[lPercent])+'%';
  //
  imgDelete.Visible := (TaskDatamodule.GetChildrenCount(FTaskRecord.ProjectNum, FTaskRecord.TaskNum)=0);
  imgArchive.Visible := (lChildren>0) and (lDone=lChildren);
  //
  layVisible.AnimateFloatDelay('opacity',1,0.5,aDelay);
end;


procedure TfrmTarget.txtDescriptionClick(Sender: TObject);
begin
  frmTasks.SelectScreenTarget(FTaskNum);
end;

procedure TfrmTarget.SelectTarget;
begin
  rectBody.Fill.Color := FProjectColor;
  FSelected := True;
  //
  txtDescription.OnMouseEnter := nil;
  txtDescription.OnMouseLeave := nil;
  txtProgress1.OnMouseEnter := nil;
  txtProgress1.OnMouseLeave := nil;
  txtProgress2.OnMouseEnter := nil;
  txtProgress2.OnMouseLeave := nil;
end;

procedure TfrmTarget.SetOnDeleteClick(aEvent: TNotifyEvent);
begin
  imgDelete.OnClick := aEvent;
end;

procedure TfrmTarget.SetOnArchiveClick(aEvent: TNotifyEvent);
begin
  imgArchive.OnClick := aEvent;
end;

procedure TfrmTarget.UnSelectTarget;
begin
  rectBody.Fill.Color := TAlphaColorRec.White;
  FSelected := False;
  //
  txtDescription.OnMouseEnter := txtDescriptionMouseEnter;
  txtDescription.OnMouseLeave := txtDescriptionMouseLeave;
  txtProgress1.OnMouseEnter := txtDescriptionMouseEnter;
  txtProgress1.OnMouseLeave := txtDescriptionMouseLeave;
  txtProgress2.OnMouseEnter := txtDescriptionMouseEnter;
  txtProgress2.OnMouseLeave := txtDescriptionMouseLeave;
end;

procedure TfrmTarget.txtDescriptionMouseEnter(Sender: TObject);
begin
  rectBody.Fill.Color := TAlphaColorRec.Snow;
end;

procedure TfrmTarget.txtDescriptionMouseLeave(Sender: TObject);
begin
  rectBody.Fill.Color := TAlphaColorRec.White;
end;

end.
