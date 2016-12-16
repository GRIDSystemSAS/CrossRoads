unit fmProject;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.Objects,
  unStructure, FMX.Filter.Effects, FMX.Effects;

type
  TfrmProject = class(TForm)
    layBody: TLayout;
    txtProjectName: TText;
    layTitle: TLayout;
    layDesc: TLayout;
    txtDescription: TText;
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
  frmProject: TfrmProject;

implementation

uses fmProjectModify, unGlobal, fmWelcomeMain;

{$R *.fmx}

procedure TfrmProject.Initialize;
begin
  layVisible.Visible := False;
  layVisible.Opacity := 0;
  txtProjectName.Text := '';
  txtDescription.Text := '';
end;

procedure TfrmProject.Initialize(aProject: TProjectRecord; aDelay: single);
begin
  FProject := aProject;
  layVisible.Visible := True;
  txtProjectName.Text := FProject.ProjectName;
  txtDescription.Text := FProject.Description;
  rectColor.Fill.Color := FProject.ColorID;
  layVisible.AnimateFloatDelay('opacity',1,0.5,aDelay);
end;

procedure TfrmProject.txtProjectNameClick(Sender: TObject);
begin
  frmWelcomeMain.InitializeCurrentProject(FProject.ProjectNum);
end;

procedure TfrmProject.txtProjectNameMouseEnter(Sender: TObject);
begin
  txtProjectName.Fill.Color := TAlphaColorRec.Black;
end;

procedure TfrmProject.txtProjectNameMouseLeave(Sender: TObject);
begin
  txtProjectName.Fill.Color := TAlphaColorRec.Dimgray;
end;

end.
