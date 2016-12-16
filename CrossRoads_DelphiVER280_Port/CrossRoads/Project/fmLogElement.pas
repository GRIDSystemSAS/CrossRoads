unit fmLogElement;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Layouts,
  unStructure;

type
  TfrmLogElement = class(TForm)
    layBody: TLayout;
    layVisible: TLayout;
    txtDate: TText;
    txtDescription: TText;
  private
    FUserLogRecord: TUserLogRecord;
  public
    procedure Initialize; overload;
    procedure Initialize(aLogRec: TUserLogRecord; aDelay: single= 0); overload;
    procedure RefreshData(aDelay: single);
  end;

var
  frmLogElement: TfrmLogElement;

implementation

uses unGlobal;

{$R *.fmx}

procedure TfrmLogElement.Initialize;
begin
  layVisible.Visible := False;
  layVisible.Opacity := 0;
  txtDate.Text := '';
  txtDescription.Text := '';
end;

procedure TfrmLogElement.Initialize(aLogRec: TUserLogRecord; aDelay: single);
begin
  FUserLogRecord := aLogRec;
  layVisible.Visible := True;
  //
  RefreshData(aDelay);
end;

procedure TfrmLogElement.RefreshData(aDelay: single);
begin
  txtDate.Text := Format('%s',[FormatDatetime(CST_FormatDatetime, FUserLogRecord.CreationDate)]);
  txtDescription.Text := FUserLogRecord.Description;
  //
  layVisible.AnimateFloatDelay('opacity',1,0.5,aDelay);
end;

end.
