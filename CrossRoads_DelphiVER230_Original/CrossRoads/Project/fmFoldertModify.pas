unit fmFoldertModify;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Layouts,
  FMX.Filter.Effects, unStructure, FMX.Memo, FMX.Edit, FMX.Colors, FMX.ListBox,
  FMX.Ani;

type
  TfrmFolderModify = class(TForm)
    pnlBody: TRectangle;
    layHeader: TLayout;
    imgLogo: TImage;
    rectline: TRectangle;
    txtTitle: TText;
    layBottom: TLayout;
    imgOK: TImage;
    imgCancel: TImage;
    layButtons: TLayout;
    MonochromeEffect1: TMonochromeEffect;
    MonochromeEffect2: TMonochromeEffect;
    lblFolder: TLabel;
    edtFolder: TEdit;
    Text1: TText;
    procedure imgOKClick(Sender: TObject);
    procedure imgCancelClick(Sender: TObject);
    procedure edtFolderKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
  private
    FFolder: string;
    procedure CheckLimitFolderName;
  public
    function Show(var aFolder: string): Boolean;
  end;

var
  frmFolderModify: TfrmFolderModify;

implementation

uses unGlobal, strUtils;

{$R *.fmx}

procedure TfrmFolderModify.CheckLimitFolderName;
begin
  edtFolder.Text := UpperCase(edtFolder.Text);
  edtFolder.Text := LeftStr(edtFolder.Text, 15);
end;

function TfrmFolderModify.Show(var aFolder: string): Boolean;
begin
  FFolder := aFolder;
  edtFolder.Text := FFolder;
  //
  edtFolder.SetFocus;
  result := (ShowModal = mrOK);
  //
  aFolder := edtFolder.Text;
end;

procedure TfrmFolderModify.edtFolderKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  /// TO-DO : check caractères spéciaux à ajouter...
  if KeyChar = ' ' then
    KeyChar := '_';
  CheckLimitFolderName;
end;

procedure TfrmFolderModify.imgCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmFolderModify.imgOKClick(Sender: TObject);
begin
  CheckLimitFolderName;
  ModalResult := mrOK;
end;

end.
