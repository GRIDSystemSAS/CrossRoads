unit fmCurrentProject;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.Objects,
  unStructure, FMX.Filter.Effects, FMX.Effects;

type
  TfrmCurrentProject = class(TForm)
    layBody: TLayout;
    imgPlay: TImage;
    txtProjectName: TText;
    layDesc: TLayout;
    txtDescription: TText;
    imgModif: TImage;
    MonochromeEffect1: TMonochromeEffect;
    layDescLeft: TLayout;
    layDescBody: TLayout;
    rectLeft: TRectangle;
    layDescBottom: TLayout;
    txtDetails: TText;
    rectBody: TRectangle;
    layButtons: TLayout;
    rectDesc: TRectangle;
    layCenter: TLayout;
    txtProgress1: TText;
    txtProgress2: TText;
    imgArchive: TImage;
    MonochromeEffect2: TMonochromeEffect;
    procedure imgModifClick(Sender: TObject);
    procedure imgModifMouseEnter(Sender: TObject);
    procedure imgModifMouseLeave(Sender: TObject);
    procedure imgArchiveMouseEnter(Sender: TObject);
    procedure imgArchiveMouseLeave(Sender: TObject);
  private
    FProject: TProjectRecord;
    procedure SetOnArchiveClick(aEvent: TNotifyEvent);
  public
    procedure Initialize(aProject: TProjectRecord);
    property OnArchiveClick: TNotifyEvent write SetOnArchiveClick;
  end;

var
  frmCurrentProject: TfrmCurrentProject;

implementation

uses fmProjectModify, unGlobal, fmWelcomeMain;

{$R *.fmx}

procedure TfrmCurrentProject.imgArchiveMouseEnter(Sender: TObject);
begin
  frmWelcomeMain.ShowHint('Archive a project...');
end;

procedure TfrmCurrentProject.imgArchiveMouseLeave(Sender: TObject);
begin
  frmWelcomeMain.HideHint;
end;

procedure TfrmCurrentProject.imgModifClick(Sender: TObject);
begin
  if frmProjectModify.Show(GlobalVar.CurrentProject.Data) then
  begin
    GlobalVar.CurrentProject.RefreshData;
    frmWelcomeMain.InitializeCurrentProject(GlobalVar.CurrentProject.Data.ProjectNum);
  end;
end;

procedure TfrmCurrentProject.imgModifMouseEnter(Sender: TObject);
begin
  frmWelcomeMain.ShowHint('Modify a project...');
end;

procedure TfrmCurrentProject.imgModifMouseLeave(Sender: TObject);
begin
  frmWelcomeMain.HideHint;
end;

procedure TfrmCurrentProject.Initialize(aProject: TProjectRecord);
var
  lDone, lChildren: integer;
  lPercent: integer;
begin
  FProject := aProject;
  txtProjectName.Text := FProject.ProjectName;
  txtDescription.Text := FProject.Description;
  rectLeft.Fill.Color := FProject.ColorID;
  //
  txtDetails.Text := Format('%d targets, %d to-do, %d scheduled',
                             [TaskDatamodule.GetTargetCount(FProject.ProjectNum),
                              TaskDatamodule.GetToDoCount(FProject.ProjectNum),
                              TaskDatamodule.GetScheduledCount(FProject.ProjectNum)]);
  //
  lDone := TaskDatamodule.GetDoneCount(FProject.ProjectNum);
  lChildren := TaskDatamodule.GetToDoCount(FProject.ProjectNum);
  txtProgress2.Text := Format('%d/%d',[ lDone, lChildren]);
  //
  if (lChildren=0) then
    lPercent := 0
  else
    lPercent := round(lDone/lChildren*100);
  txtProgress1.Text := Format('%d',[lPercent])+'%';
  //
  imgArchive.Visible := (lChildren>0) and (lChildren=lDone)
                        and (GlobalVar.ProjectList.Count>1);
end;

procedure TfrmCurrentProject.SetOnArchiveClick(aEvent: TNotifyEvent);
begin
  imgArchive.OnClick := aEvent;
end;

end.
