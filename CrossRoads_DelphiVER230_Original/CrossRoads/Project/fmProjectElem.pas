unit fmProjectElem;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.Objects,
  unStructure, FMX.Filter.Effects, FMX.Effects;

type
  TfrmProjectElem = class(TForm)
    layBody: TLayout;
    txtProjectName: TText;
    layTitle: TLayout;
    layDesc: TLayout;
    rectColor: TRectangle;
    layDescLeft: TLayout;
    layDescBody: TLayout;
    LayVisible: TLayout;
    procedure txtProjectNameClick(Sender: TObject);
    procedure txtProjectNameMouseEnter(Sender: TObject);
    procedure txtProjectNameMouseLeave(Sender: TObject);
  private
    FProject: TProjectRecord;
  public
    procedure Initialize; overload;
    procedure Initialize(aProject: TProjectRecord; aDelay: single= 0); overload;
  end;

var
  frmProjectElem: TfrmProjectElem;

implementation

uses fmProjectModify, unGlobal, fmWelcomeMain;

{$R *.fmx}

procedure TfrmProjectElem.Initialize;
begin
  layVisible.Visible := False;
  layVisible.Opacity := 0;
  txtProjectName.Text := '';
end;

procedure TfrmProjectElem.Initialize(aProject: TProjectRecord; aDelay: single);
begin
  FProject := aProject;
  layVisible.Visible := True;
  txtProjectName.Text := FProject.ProjectName;
  rectColor.Fill.Color := FProject.ColorID;
  layVisible.AnimateFloatDelay('opacity',1,0.5,aDelay);
end;

procedure TfrmProjectElem.txtProjectNameClick(Sender: TObject);
begin
  if (FProject.ProjectNum<>GlobalVar.CurrentProjectNum) then
    frmWelcomeMain.InitializeCurrentProject(FProject.ProjectNum);
end;

procedure TfrmProjectElem.txtProjectNameMouseEnter(Sender: TObject);
begin
  txtProjectName.Fill.Color := TAlphaColorRec.Black;
end;

procedure TfrmProjectElem.txtProjectNameMouseLeave(Sender: TObject);
begin
  txtProjectName.Fill.Color := TAlphaColorRec.Dimgray;
end;

end.
