unit fmLinkElement;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Layouts,
  unStructure, fmTaskPending, FMX.Filter.Effects, fmTargetTodo;

type
  TfrmLinkElement = class(TForm)
    layBody: TLayout;
    layVisible: TLayout;
    layGoto: TLayout;
    imgGoto: TImage;
    txtDescription: TText;
    imgDelete: TImage;
    MonochromeEffect1: TMonochromeEffect;
    layDelete: TLayout;
    MonochromeEffect2: TMonochromeEffect;
    procedure txtDescriptionMouseEnter(Sender: TObject);
    procedure txtDescriptionMouseLeave(Sender: TObject);
    procedure txtDescriptionClick(Sender: TObject);
    procedure imgDeleteClick(Sender: TObject);
  private
    FTaskRecord: TTaskRecord;
    FParent: TfrmTargetTodo;
  public
    procedure Initialize; overload;
    procedure Initialize(aTaskRec: TTaskRecord; aDelay: single= 0); overload;
    procedure RefreshData(aDelay: single);
    //
    property Parent: TfrmTargetTodo read FParent write FParent;
  end;

var
  frmLinkElement: TfrmLinkElement;

implementation

uses unGlobal, dmTypeList, fmTargetModify;

{$R *.fmx}

procedure TfrmLinkElement.Initialize;
begin
  layVisible.Visible := False;
  layVisible.Opacity := 0;
  txtDescription.Text := '';
end;

procedure TfrmLinkElement.imgDeleteClick(Sender: TObject);
begin
  if MessageDlg('Delete link ?', TMsgDlgType.mtConfirmation, mbYesNo, 0)=mrNo then exit;
  //
  TaskDatamodule.DeleteTask(FTaskRecord.TaskNum);
  FParent.Loadlinks;
end;

procedure TfrmLinkElement.Initialize(aTaskRec: TTaskRecord; aDelay: single);
begin
  FTaskRecord := aTaskRec;
  layVisible.Visible := True;
  //
  RefreshData(aDelay);
end;

procedure TfrmLinkElement.RefreshData(aDelay: single);
begin
  txtDescription.Text := FTaskRecord.TaskName;
  //
  layVisible.AnimateFloatDelay('opacity',1,0.5,aDelay);
end;

procedure TfrmLinkElement.txtDescriptionClick(Sender: TObject);
begin
  if frmTargetModify.Show(FTaskRecord, CST_TargetTask) then
    FParent.Loadlinks;
end;

procedure TfrmLinkElement.txtDescriptionMouseEnter(Sender: TObject);
begin
  txtDescription.Font.Style := [TFontStyle.fsBold];
end;

procedure TfrmLinkElement.txtDescriptionMouseLeave(Sender: TObject);
begin
  txtDescription.Font.Style := [];
end;

end.
