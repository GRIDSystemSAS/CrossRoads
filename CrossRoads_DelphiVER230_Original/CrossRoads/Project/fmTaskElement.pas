unit fmTaskElement;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Layouts,
  unStructure, fmTaskPending, FMX.Filter.Effects;

type
  TfrmTaskElement = class(TForm)
    layBody: TLayout;
    layVisible: TLayout;
    layCheckBox: TLayout;
    rectCheckBox: TRectangle;
    imgCheckBox: TImage;
    txtDescription: TText;
    imgDelete: TImage;
    MonochromeEffect1: TMonochromeEffect;
    layDelete: TLayout;
    procedure imgCheckBoxClick(Sender: TObject);
    procedure txtDescriptionMouseEnter(Sender: TObject);
    procedure txtDescriptionMouseLeave(Sender: TObject);
    procedure txtDescriptionClick(Sender: TObject);
    procedure imgDeleteClick(Sender: TObject);
  private
    FTaskRecord: TTaskRecord;
    FParent: TFrmTaskPending;
    FIsReadOnly: Boolean;
  public
    procedure Initialize; overload;
    procedure Initialize(aTaskRec: TTaskRecord; aDelay: single= 0); overload;
    procedure RefreshData(aDelay: single);
    //
    property Parent: TFrmTaskPending read FParent write FParent;
    property IsReadOnly: Boolean read FIsReadOnly write FIsReadOnly;
  end;

var
  frmTaskElement: TfrmTaskElement;

implementation

uses unGlobal, dmTypeList, fmTargetModify;

{$R *.fmx}

procedure TfrmTaskElement.Initialize;
begin
  layVisible.Visible := False;
  layVisible.Opacity := 0;
  txtDescription.Text := '';
  imgCheckBox.Visible := False;
  FIsReadOnly := False;
end;

procedure TfrmTaskElement.imgCheckBoxClick(Sender: TObject);
var
  lDescLog: TStringList;
  lCheckedStr: string;
begin
  if (FTaskRecord.Status <> staDone) then
  begin
    FTaskRecord.Status := staDone;
    FTaskRecord.DoneDate := now;
    lCheckedStr := 'checked';
  end
  else
  begin
   FTaskRecord.Status := staCreated;
   lCheckedStr := 'unchecked';
  end;
  TaskDatamodule.UpdateTask(FTaskRecord);
  //
  lDescLog := TStringList.Create;
  lDescLog.Add(Format('A task has been %s:',[lCheckedStr]));
  lDescLog.Add(Format('- %s',[FTaskRecord.TaskName]));
  UserLogDatamodule.AddUserLog(CST_UserLogType_Info, lDescLog.Text);
  lDescLog.Free;
  //
  RefreshData(0);
end;

procedure TfrmTaskElement.imgDeleteClick(Sender: TObject);
var
  lDescLog: TStringList;
begin
  if MessageDlg('Delete task ?', TMsgDlgType.mtConfirmation, mbYesNo, 0)=mrNo then exit;
  //
  TaskDatamodule.DeleteTask(FTaskRecord.TaskNum);
  //
  lDescLog := TStringList.Create;
  lDescLog.Add('A task has been deleted:');
  lDescLog.Add(Format('- %s',[FTaskRecord.TaskName]));
  UserLogDatamodule.AddUserLog(CST_UserLogType_Info, lDescLog.Text);
  lDescLog.Free;
  //
  FParent.LoadTasks;
end;

procedure TfrmTaskElement.Initialize(aTaskRec: TTaskRecord; aDelay: single);
begin
  FTaskRecord := aTaskRec;
  layVisible.Visible := True;
  //
  RefreshData(aDelay);
end;

procedure TfrmTaskElement.RefreshData(aDelay: single);
begin
  txtDescription.Text := FTaskRecord.TaskName;
  imgCheckBox.Visible := (FTaskRecord.Status = staDone);
  imgDelete.Visible := (FTaskRecord.Status <> staDone);
  layDelete.Visible := not FIsReadOnly;
  if FIsReadOnly then
  begin
    txtDescription.Font.Style := [];
    txtDescription.Fill.Color := TAlphaColorRec.Black;
    txtDescription.OnClick := nil;
    txtDescription.OnMouseEnter := nil;
    txtDescription.OnMouseLeave := nil;
    imgCheckBox.OnClick := nil;
    rectCheckBox.OnClick := nil;
  end
  else
  if (FTaskRecord.Status = staDone) then
  begin
    txtDescription.Font.Style := [TFontStyle.fsStrikeOut];
    txtDescription.Fill.Color := TAlphaColorRec.Gray;
    txtDescription.OnClick := nil;
    txtDescription.OnMouseEnter := nil;
    txtDescription.OnMouseLeave := nil;
    imgCheckBox.OnClick := imgCheckBoxClick;
    rectCheckBox.OnClick := imgCheckBoxClick;
  end
  else
  begin
    txtDescription.Font.Style := [];
    txtDescription.Fill.Color := TAlphaColorRec.Black;
    txtDescription.OnClick := txtDescriptionClick;
    txtDescription.OnMouseEnter := txtDescriptionMouseEnter;
    txtDescription.OnMouseLeave := txtDescriptionMouseLeave;
    imgCheckBox.OnClick := imgCheckBoxClick;
    rectCheckBox.OnClick := imgCheckBoxClick;
  end;
  //
  layVisible.AnimateFloatDelay('opacity',1,0.5,aDelay);
end;

procedure TfrmTaskElement.txtDescriptionClick(Sender: TObject);
begin
  if frmTargetModify.Show(FTaskRecord, CST_TargetTask) then
    FParent.LoadTasks;
end;

procedure TfrmTaskElement.txtDescriptionMouseEnter(Sender: TObject);
begin
  txtDescription.Font.Style := [TFontStyle.fsBold];
end;

procedure TfrmTaskElement.txtDescriptionMouseLeave(Sender: TObject);
begin
  txtDescription.Font.Style := [];
end;

end.
