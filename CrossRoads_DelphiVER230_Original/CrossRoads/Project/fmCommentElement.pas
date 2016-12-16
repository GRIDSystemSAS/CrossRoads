unit fmCommentElement;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Layouts,
  unStructure, FMX.Filter.Effects, FMX.Ani, fmTargetTodo;

type
  TfrmCommentElement = class(TForm)
    layBody: TLayout;
    layVisible: TLayout;
    layTop: TLayout;
    txtLabel: TText;
    txtCreationDate: TText;
    txtDescription: TText;
    rectBody: TRectangle;
    rectTop: TRectangle;
    layBottom: TLayout;
    layButton: TLayout;
    imgEdit: TImage;
    FloatAnimation1: TFloatAnimation;
    MonochromeEffect1: TMonochromeEffect;
    imgDelete: TImage;
    FloatAnimation2: TFloatAnimation;
    MonochromeEffect2: TMonochromeEffect;
    procedure imgEditClick(Sender: TObject);
    procedure imgDeleteClick(Sender: TObject);
  private
    FTaskRecord: TTaskRecord;
    FParent: TfrmTargetTodo;
    FIsReadOnly: Boolean;
  public
    procedure Initialize; overload;
    procedure Initialize(aTaskRec: TTaskRecord; aDelay: single= 0); overload;
    procedure RefreshData(aDelay: single);
    //
    property Parent: TfrmTargetTodo read FParent write FParent;
    property IsReadOnly: Boolean read FIsReadOnly write FIsReadOnly;
  end;

var
  frmCommentElement: TfrmCommentElement;

implementation

uses unGlobal, dmTypeList, fmTargetModify;

{$R *.fmx}

procedure TfrmCommentElement.Initialize;
begin
  layVisible.Visible := False;
  layVisible.Opacity := 0;
  txtLabel.Text := '';
  txtCreationDate.Text := '';
  txtDescription.Text := '';
  FIsReadOnly := False;
end;

procedure TfrmCommentElement.imgDeleteClick(Sender: TObject);
begin
  if MessageDlg('Delete comment ?', TMsgDlgType.mtConfirmation, mbYesNo, 0)=mrNo then exit;
  //
  TaskDatamodule.DeleteTask(FTaskRecord.TaskNum);
  FParent.LoadComments;
end;

procedure TfrmCommentElement.imgEditClick(Sender: TObject);
begin
  if frmTargetModify.Show(FTaskRecord, CST_TargetComment) then
    FParent.LoadComments;
end;

procedure TfrmCommentElement.Initialize(aTaskRec: TTaskRecord; aDelay: single);
begin
  FTaskRecord := aTaskRec;
  layVisible.Visible := True;
  //
  RefreshData(aDelay);
end;

procedure TfrmCommentElement.RefreshData(aDelay: single);
var
  lStrLines: TStringList;
begin
  txtLabel.Text := FTaskRecord.TaskName;
  txtCreationDate.Text := Format('created on %s',[FormatDatetime(CST_FormatDatetime, FTaskRecord.CreationDate)]);
  txtDescription.Text := FTaskRecord.Description;
  lStrLines := TStringList.Create;
  lStrLines.Text := FTaskRecord.Description;
  layBody.Height := 80 + 10*lStrLines.Count;
  //
  layButton.Visible := not FIsReadOnly;
  //
  layVisible.AnimateFloatDelay('opacity',1,0.5,aDelay);
end;

end.
