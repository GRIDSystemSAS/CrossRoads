//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
// TLSNMenu v.1.0
// by Laurent Sengmany - 30.10.2011
//-----------------------------------------------------------------------------
// Description : this class will help you to create 2-levels animated menus
// How-to :
//      // declare and create menu:
//      FLSNMenu: TLSNMenu;
//      FLSNMenu := TLSNMenu.Create(controlMenu);
//      // customize your menu
//      FLSNMenu.ScreenStrokeColor := TAlphaColorRec.White;
//      // add a header:
//      FLSNMenu.AddHeader('Header 1');
//      // add submenu and attach a panel that contains elements to show:
//      lSubMenu := FLSNMenu.AddSubMenu('Screen 1', controlScreenBody);
//      lSubMenu.ScreenPanel := lForm.pnlScreen1;
//      // build menu
//      FLSNMenu.BuildControls;
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------

unit unLSNMenu;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Ani,
  FMX.Menus, FMX.Effects, FMX.Layouts, FMX.STDCTRLS;

type
  TLSNSubMenu = class
    private
      FText: string;
      FParent: TControl;
      FControl: TLabel;
      FScreenRect: TRectangle;
      FScreenPanel: TPanel;
      //
      FScreenBackgroundColor, FScreenStrokeColor: TColor;
    protected

    public
      constructor Create(aParent: TControl);
      destructor Destroy; override;
      procedure BuildControls;
      procedure ShowScreen;
      procedure HideScreen;
      procedure Resize;
      //
      property Text: string read FText write FText;
      property Parent: TControl read FParent write FParent;
      property Control: TLabel read FControl write FControl;
      property ScreenPanel: TPanel read FScreenPanel write FScreenPanel;
      //
      property ScreenBackgroundColor: TColor read FScreenBackgroundColor write FScreenBackgroundColor;
      property ScreenStrokeColor: TColor read FScreenStrokeColor write FScreenStrokeColor;
  end;

  TLSNHeaderMenu = class
    private
      FText: string;
      FListSubMenu: TList;
      //
      FBackgroundColor: TColor;
      FMenuActiveSubMenuColor, FMenuInactiveSubMenuColor: TColor;
      FMenuFocusedSubMenuColor: TColor;
      FSubMenuFontFamily: string;
      FSubMenuFontSize: extended;
      FSubMenuFontIsBold: Boolean;
      // controls
      FParent: TControl;
      FControl: TLabel;
      FSubMenuRect: TRectangle;
    protected
      //
    public
      constructor Create(aParent: TControl);
      destructor Destroy; override;
      procedure BuildControls;
      procedure HideSubMenus;
      procedure ShowSubMenus(aPositionX: single);
      //
      function AddSubMenu(aText: string; aScreenParent: TControl): TLSNSubMenu;
      //
      property Text: string read FText write FText;
      property Parent: TControl read FParent write FParent;
      property Control: TLabel read FControl write FControl;
      property ListSubMenu: TList read FListSubMenu;
      //
      property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor;
      property MenuActiveSubMenuColor: TColor read FMenuActiveSubMenuColor write FMenuActiveSubMenuColor;
      property MenuInactiveSubMenuColor: TColor read FMenuInactiveSubMenuColor write FMenuInactiveSubMenuColor;
      property MenuFocusedSubMenuColor: TColor read FMenuFocusedSubMenuColor write FMenuFocusedSubMenuColor;
      property SubMenuFontFamily: string read FSubMenuFontFamily write FSubMenuFontFamily;
      property SubMenuFontSize: extended read FSubMenuFontSize write FSubMenuFontSize;
      property SubMenuFontIsBold: Boolean read FSubMenuFontIsBold write FSubMenuFontIsBold;

  end;

  TLSNMenu = class
    private
      FListHeaderMenu: TList;
      FCurrentHeaderMenu: TLSNHeaderMenu;
      FListSubMenu: TList;
      FCurrentSubMenu: TLSNSubMenu;
      //
      FBackgroundColor: TColor;
      FMenuActiveHeaderColor, FMenuInactiveHeaderColor: TColor;
      FMenuFocusedHeaderColor: TColor;
      FMenuActiveSubMenuColor, FMenuInactiveSubMenuColor: TColor;
      FMenuFocusedSubMenuColor: TColor;
      FScreenBackgroundColor, FScreenStrokeColor: TColor;
      FSubMenuFontFamily: string;
      FSubMenuFontSize: extended;
      FSubMenuFontIsBold: Boolean;
      FHeaderFontFamily: string;
      FHeaderFontSize: extended;
      FHeaderFontIsBold: Boolean;
      // controls
      FParent: TControl;
      FHeaderRect: TRectangle;
    protected
      // events
      procedure HeaderMouseEnter(Sender: TObject);
      procedure HeaderMouseLeave(Sender: TObject);
      procedure HeaderClick(Sender: TObject);
      procedure SubMenuMouseEnter(Sender: TObject);
      procedure SubMenuMouseLeave(Sender: TObject);
      procedure SubMenuClick(Sender: TObject);
    public
      constructor Create(aParent: TControl);
      destructor Destroy; override;
      procedure BuildControls;
      //
      function AddHeader(aText: string): integer;
      function AddSubMenu(aText: string; aScreenParent: TControl; aHeaderIndex: integer = -1): TLSNSubMenu;
      function AddHiddenSubMenu(aText: string; aScreenParent: TControl): TLSNSubMenu;
      procedure LaunchSubMenu(aIndex: integer);
      procedure Resize;
      //
      property Parent: TControl read FParent write FParent;
      //
      property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor;
      property MenuActiveHeaderColor: TColor read FMenuActiveHeaderColor write FMenuActiveHeaderColor;
      property MenuInactiveHeaderColor: TColor read FMenuInactiveHeaderColor write FMenuInactiveHeaderColor;
      property MenuFocusedHeaderColor: TColor read FMenuFocusedHeaderColor write FMenuFocusedHeaderColor;
      property MenuActiveSubMenuColor: TColor read FMenuActiveSubMenuColor write FMenuActiveSubMenuColor;
      property MenuInactiveSubMenuColor: TColor read FMenuInactiveSubMenuColor write FMenuInactiveSubMenuColor;
      property MenuFocusedSubMenuColor: TColor read FMenuFocusedSubMenuColor write FMenuFocusedSubMenuColor;
      property ScreenBackgroundColor: TColor read FScreenBackgroundColor write FScreenBackgroundColor;
      property ScreenStrokeColor: TColor read FScreenStrokeColor write FScreenStrokeColor;
      property SubMenuFontFamily: string read FSubMenuFontFamily write FSubMenuFontFamily;
      property SubMenuFontSize: extended read FSubMenuFontSize write FSubMenuFontSize;
      property SubMenuFontIsBold: Boolean read FSubMenuFontIsBold write FSubMenuFontIsBold;
      property HeaderFontFamily: string read FHeaderFontFamily write FHeaderFontFamily;
      property HeaderFontSize: extended read FHeaderFontSize write FHeaderFontSize;
      property HeaderFontIsBold: Boolean read FHeaderFontIsBold write FHeaderFontIsBold;
  end;

  TLSNAction = class
  private
    FContainer, FFormLayout: TLayout;
    FParent: TControl;
    FIndex: integer;
  public
    constructor Create(aIndex: integer; aParent: TControl; aFormLayout: TLayout);
    destructor Destroy; override;
    //
    procedure HideScreen;
    procedure ShowScreen(aAnimationType: Integer);
    procedure Resize;
    //
    property Index: integer read FIndex write FIndex;
  end;

  TLSNActionList = class
  private
    FActionList: TList;
    FActiveAction: TLSNAction;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(aIndex: integer; aParent: TControl; aFormLayout: TLayout);
    function SearchAction(aIndex: integer): TLSNAction;
    procedure OpenScreen(aIndex: integer; aAnimationType: Integer = 0);
    procedure Resize;
  end;

implementation

{$REGION 'TLSNSubMenu'}

constructor TLSNSubMenu.Create(aParent: TControl);
begin
  FParent := aParent;
  FScreenPanel := nil;
  FScreenBackgroundColor := TAlphaColorRec.White;
  FScreenStrokeColor := TAlphaColorRec.Darkgray;
end;

destructor TLSNSubMenu.Destroy;
begin

end;

procedure TLSNSubMenu.BuildControls;
begin
  // Screen rectangle
  FScreenRect := TRectangle.Create(nil);
  FScreenRect.Parent := FParent;
  FScreenRect.Position.X := FParent.Width + 10;
  FScreenRect.Position.Y := 0;
  FScreenRect.Height := FParent.Height;
  FScreenRect.Width := FParent.Width;
  FScreenRect.ClipChildren := True;
  FScreenRect.Fill.Color := FScreenBackgroundColor;
  FScreenRect.Stroke.Color := FScreenStrokeColor;

  // embedd screen
  if FScreenPanel<>nil then
  begin
    FScreenPanel.Parent := FScreenRect;
    FScreenPanel.Align := TAlignLayout.alClient;
  end;
end;

procedure TLSNSubMenu.ShowScreen;
begin
  FScreenRect.Height := FParent.Height;
  FScreenRect.Width := FParent.Width;
  FScreenRect.BringToFront;
  FScreenRect.Scale.X := 1;
  FScreenRect.Scale.Y := 1;
  FScreenRect.Position.X := FParent.Width + 10;
  //
  FScreenRect.AnimateFloatDelay('Position.X',0,1.5,0.5,TAnimationType.&In,TInterpolationType.Quartic);
end;

procedure TLSNSubMenu.HideScreen;
begin
  FScreenRect.AnimateFloatDelay('Scale.X',0.8,0.5,0,TAnimationType.&In,TInterpolationType.Linear);
  FScreenRect.AnimateFloatDelay('Scale.Y',0.8,0.5,0,TAnimationType.&In,TInterpolationType.Linear);
  FScreenRect.AnimateFloatDelay('Position.X',FParent.Width+10,1.5,0.3,TAnimationType.&In,TInterpolationType.Quartic);

end;

procedure TLSNSubMenu.Resize;
begin
  FScreenRect.Height := FParent.Height;
  FScreenRect.Width := FParent.Width;
  if FScreenRect.Position.X > 0 then
    FScreenRect.Position.X := FParent.Width + 10;
end;


{$ENDREGION}


{$REGION 'TLSNHeaderMenu'}

constructor TLSNHeaderMenu.Create(aParent: TControl);
begin
  FListSubMenu := TList.Create;
  FParent := aParent;
  // default colors
  FBackgroundColor := TAlphaColorRec.White;
  FMenuActiveSubMenuColor :=  TAlphaColorRec.Darkred;
  FMenuInactiveSubMenuColor := TAlphaColorRec.Lightgray;
  FMenuFocusedSubMenuColor := TAlphaColorRec.Crimson;
  FSubMenuFontFamily := 'Verdana';
  FSubMenuFontSize := 11;
  FSubMenuFontIsBold := False;
end;

destructor TLSNHeaderMenu.Destroy;
begin
  FListSubMenu.Free;
end;


function TLSNHeaderMenu.AddSubMenu(aText: string; aScreenParent: TControl): TLSNSubMenu;
var
  lMenu: TLSNSubMenu;
begin
  lMenu := TLSNSubMenu.Create(aScreenParent);
  lMenu.Text := aText;
  FListSubMenu.Add(lMenu);
  result := lMenu;
end;


procedure TLSNHeaderMenu.BuildControls;
var
  lLabel: TLabel;
  i: Integer;
  lMenu: TLSNSubMenu;
  lWidth: single;
begin
  // SubMenu rectangle
  FSubMenuRect := TRectangle.Create(nil);
  FSubMenuRect.Parent := FParent;
  FSubMenuRect.Height := 16;
  FSubMenuRect.Position.Y := 18;
  FSubMenuRect.Fill.Color := FBackgroundColor;
  FSubMenuRect.Stroke.Color := TAlphaColorRec.Lightgray;
  //FSubMenuRect.Sides := [TSide.sdBottom, TSide.sdRight];
  FSubMenuRect.Sides := [TSide.sdTop];
  FSubMenuRect.Opacity := 0;
  //
  // create SubMenus controls
  for i := 0 to FListSubMenu.Count-1 do
  begin
    lMenu := FListSubMenu[i];
    //
    lLabel := TLabel.Create(nil);
    lLabel.Parent := FSubMenuRect;
    lLabel.ApplyStyleLookup;
    lLabel.Font.Family := FSubMenuFontFamily;
    lLabel.Font.Size := SubMenuFontSize;
    lLabel.TextSettings.FontColor := FMenuInactiveSubMenuColor;
    if FSubMenuFontIsBold then
      lLabel.Font.Style := [TFontStyle.fsBold];
    lLabel.Padding.Right := 5;
    lLabel.Padding.Top := 2;
    lLabel.Text := UpperCase(lMenu.Text);
    lLabel.AutoSize := True;
    lLabel.HitTest := True;
    lMenu.Control := lLabel;
    // build submenu screen panel
    lMenu.BuildControls;
  end;
  // align headers
  for i := FListSubMenu.Count-1 downto 0 do
  begin
    lMenu := FListSubMenu[i];
    lLabel := lMenu.Control;
    lLabel.Align := TAlignLayout.alLeft;
  end;
  // sum width of SubMenus
  lWidth := 0;
  for i := FListSubMenu.Count-1 downto 0 do
  begin
    lMenu := FListSubMenu[i];
    lLabel := lMenu.Control;
    lWidth := lWidth + lLabel.Width + 5;
  end;
  FSubMenuRect.Width := lWidth;
end;

procedure TLSNHeaderMenu.HideSubMenus;
begin
  FSubMenuRect.AnimateFloat('Opacity',0,0.2,TAnimationType.In,TInterpolationType.Linear);
end;

procedure TLSNHeaderMenu.ShowSubMenus(aPositionX: single);
begin
  FSubMenuRect.Position.X := aPositionX;
  FSubMenuRect.AnimateFloatDelay('Opacity',1,0.5,0.2,TAnimationType.In,TInterpolationType.Linear);
  FSubMenuRect.BringToFront;
end;


{$ENDREGION}


{$REGION 'TLSNMenu'}

constructor TLSNMenu.Create(aParent: TControl);
begin
  FListHeaderMenu := TList.Create;
  FListSubMenu := TList.Create;
  FParent := aParent;
  // default colors
  FBackgroundColor := TAlphaColorRec.White;
  FMenuActiveHeaderColor :=  TAlphaColorRec.Black;
  FMenuInactiveHeaderColor := TAlphaColorRec.Lightgray;
  FMenuFocusedHeaderColor := TAlphaColorRec.Gray;
  FMenuActiveSubMenuColor :=  TAlphaColorRec.Darkred;
  FMenuInactiveSubMenuColor := TAlphaColorRec.Gray;
  FMenuFocusedSubMenuColor := TAlphaColorRec.Crimson;
  FScreenBackgroundColor := TAlphaColorRec.White;
  FScreenStrokeColor := TAlphaColorRec.Darkgray;
  //
  FSubMenuFontFamily := 'Verdana';
  FSubMenuFontSize := 11;
  FSubMenuFontIsBold := False;
  FHeaderFontFamily := 'Franklin Gothic Medium';
  FHeaderFontSize := 15;
  FHeaderFontIsBold := False;
  //
  AddHeader('Hidden');
end;

destructor TLSNMenu.Destroy;
begin
  FListHeaderMenu.Free;
  FListSubMenu.Free;
end;

function TLSNMenu.AddHeader(aText: string): integer;
var
  lMenu: TLSNHeaderMenu;
begin
  lMenu := TLSNHeaderMenu.Create(FParent);
  lMenu.Text := aText;
  FListHeaderMenu.Add(lMenu);
  FCurrentHeaderMenu := lMenu;
  result := FListHeaderMenu.Count-1;
end;

function TLSNMenu.AddSubMenu(aText: string; aScreenParent: TControl; aHeaderIndex: integer = -1): TLSNSubMenu;
var
  lMenu: TLSNHeaderMenu;
  lSubMenu: TLSNSubMenu;
begin
  if aHeaderIndex = -1 then
    lMenu := FCurrentHeaderMenu
  else
    lMenu := FListHeaderMenu[aHeaderIndex];

  lSubMenu := lMenu.AddSubMenu(aText, aScreenParent);
  lSubMenu.ScreenBackgroundColor := FScreenBackgroundColor;
  lSubMenu.ScreenStrokeColor := FScreenStrokeColor;
  //
  FListSubMenu.Add(lSubMenu);
  result := lSubMenu;
end;

function TLSNMenu.AddHiddenSubMenu(aText: string; aScreenParent: TControl): TLSNSubMenu;
begin
  result := AddSubMenu(aText, aScreenParent, 0);
end;

procedure TLSNMenu.LaunchSubMenu(aIndex: integer);
var
  lSubMenu: TLSNSubMenu;
  lLabel: TLabel;
begin
  lSubMenu := FListSubMenu[aIndex];
  lLabel := lSubMenu.Control;
  SubMenuClick(lLabel);
end;

procedure TLSNMenu.Resize;
var
  i: Integer;
  lSubMenu: TLSNSubMenu;
begin
  for i := 0 to FListSubMenu.Count-1 do
  begin
    lSubMenu := FListSubMenu[i];
    lSubMenu.Resize;
  end;
end;


procedure TLSNMenu.BuildControls;
var
  lLabel: TLabel;
  i: Integer;
  lMenu: TLSNHeaderMenu;
  lSubMenu: TLSNSubMenu;
begin
  // Header rectangle
  FHeaderRect := TRectangle.Create(nil);
  FHeaderRect.Parent := FParent;
  FHeaderRect.Align := TAlignLayout.alTop;
  FHeaderRect.Height := 17;
  FHeaderRect.Fill.Color := FBackgroundColor;
  FHeaderRect.Sides := [];
  FHeaderRect.Opacity := 0;
  // create headers controls
  // don't show 1st header (Hidden)
  for i := 0 to FListHeaderMenu.Count-1 do
  begin
    lMenu := FListHeaderMenu[i];
    lMenu.BackgroundColor := FBackgroundColor;
    lMenu.MenuActiveSubMenuColor := FMenuActiveSubMenuColor;
    lMenu.MenuInactiveSubMenuColor := FMenuInactiveSubMenuColor;
    lMenu.MenuFocusedSubMenuColor := FMenuFocusedSubMenuColor;
    lMenu.SubMenuFontFamily := FSubMenuFontFamily;
    lMenu.SubMenuFontSize := FSubMenuFontSize;
    lMenu.SubMenuFontIsBold := FSubMenuFontIsBold;
    //
    lLabel := TLabel.Create(nil);
    lLabel.Parent := FHeaderRect;
    lLabel.ApplyStyleLookup;
    lLabel.Font.Family := FHeaderFontFamily;
    lLabel.Font.Size := FHeaderFontSize;
    if FHeaderFontIsBold then
      lLabel.Font.Style := [TFontStyle.fsBold];
    lLabel.TextSettings.FontColor := FMenuInactiveHeaderColor;
    lLabel.Padding.Right := 7;
    lLabel.Text := UpperCase(lMenu.Text);
    lLabel.AutoSize := True;
    lLabel.OnClick := HeaderClick;
    lLabel.OnMouseEnter := HeaderMouseEnter;
    lLabel.OnMouseLeave := HeaderMouseLeave;
    lLabel.HitTest := True;
    lLabel.Tag := i;
    lLabel.Visible := (i>0);
    lMenu.Control := lLabel;

    //
    lMenu.BuildControls;
  end;
  // align headers
  for i := FListHeaderMenu.Count-1 downto 0 do
  begin
    lMenu := FListHeaderMenu[i];
    lLabel := lMenu.Control;
    lLabel.Align := TAlignLayout.alLeft;
  end;
  // reaffect submenus tag and events
  for i := 0 to FListSubMenu.Count-1 do
  begin
    lSubMenu := FListSubMenu[i];
    lSubMenu.Control.Tag := i;
    lSubMenu.Control.OnClick := SubMenuClick;
    lSubMenu.Control.OnMouseEnter := SubMenuMouseEnter;
    lSubMenu.Control.OnMouseLeave := SubMenuMouseLeave;
  end;

  // start animations
  FHeaderRect.AnimateFloat('Opacity',1,1,TAnimationType.In,TInterpolationType.Linear);

  // default select
  if FListHeaderMenu.Count>1 then
  begin
    lMenu := FListHeaderMenu[1];
    HeaderClick(lMenu.Control);
  end;
end;


{$REGION 'TLSNMenu.Events'}

procedure TLSNMenu.HeaderMouseEnter(Sender: TObject);
begin
  TLabel(Sender).ApplyStyleLookup;
  TLabel(Sender).TextSettings.FontColor := FMenuFocusedHeaderColor;
end;

procedure TLSNMenu.HeaderMouseLeave(Sender: TObject);
begin
  TLabel(Sender).ApplyStyleLookup;
  TLabel(Sender).TextSettings.FontColor := FMenuInactiveHeaderColor;
end;

procedure TLSNMenu.HeaderClick(Sender: TObject);
var
  lLabel: TLabel;
  i: Integer;
  lMenu, lOldMenu: TLSNHeaderMenu;
  lMenuX: single;
begin
  // init headers color
  for i := FListHeaderMenu.Count-1 downto 0 do
  begin
    lMenu := FListHeaderMenu[i];
    lLabel := lMenu.Control;
    lLabel.ApplyStyleLookup;
    lLabel.TextSettings.FontColor := FMenuInactiveHeaderColor;
  end;
  //
  TLabel(Sender).ApplyStyleLookup;
  TLabel(Sender).TextSettings.FontColor := FMenuActiveHeaderColor;
  //
  lOldMenu := FCurrentHeaderMenu;
  lMenu := FListHeaderMenu[TLabel(Sender).Tag];
  FCurrentHeaderMenu := lMenu;
  lMenuX := lMenu.Control.Position.X;
  // events
  TLabel(Sender).OnClick := nil;
  TLabel(Sender).OnMouseEnter := nil;
  TLabel(Sender).OnMouseLeave := nil;
  lLabel := lOldMenu.Control;
  lLabel.OnClick := HeaderClick;
  lLabel.OnMouseEnter := HeaderMouseEnter;
  lLabel.OnMouseLeave := HeaderMouseLeave;
  //
  lOldMenu.HideSubMenus;
  lMenu.ShowSubMenus(lMenuX);
end;

procedure TLSNMenu.SubMenuMouseEnter(Sender: TObject);
begin
  TLabel(Sender).ApplyStyleLookup;
  TLabel(Sender).TextSettings.FontColor := FMenuFocusedSubMenuColor;
end;

procedure TLSNMenu.SubMenuMouseLeave(Sender: TObject);
begin
  TLabel(Sender).ApplyStyleLookup;
  TLabel(Sender).TextSettings.FontColor := FMenuInactiveSubMenuColor;
end;

procedure TLSNMenu.SubMenuClick(Sender: TObject);
var
  lOldSubMenu, lSubMenu: TLSNSubMenu;
  lLabel: TLabel;
begin
  //
  TLabel(Sender).ApplyStyleLookup;
  TLabel(Sender).TextSettings.FontColor := FMenuActiveSubMenuColor;
  //
  lOldSubMenu := FCurrentSubMenu;
  lSubMenu := FListSubMenu[TLabel(Sender).Tag];
  FCurrentSubMenu := lSubMenu;
  //
  TLabel(Sender).OnClick := nil;
  TLabel(Sender).OnMouseEnter := nil;
  TLabel(Sender).OnMouseLeave := nil;
  if lOldSubMenu<>nil then
  begin
    lLabel := lOldSubMenu.Control;
    lLabel.ApplyStyleLookup;
    lLabel.TextSettings.FontColor := FMenuInactiveSubMenuColor;
    lLabel.OnClick := SubMenuClick;
    lLabel.OnMouseEnter := SubMenuMouseEnter;
    lLabel.OnMouseLeave := SubMenuMouseLeave;
  end;
  //
  if lOldSubMenu<>nil then
    lOldSubMenu.HideScreen;
  lSubMenu.ShowScreen;
end;


{$ENDREGION}

{$ENDREGION}

{$REGION 'TLSNAction'}

constructor TLSNAction.Create(aIndex: integer; aParent: TControl; aFormLayout: TLayout);
begin
  FParent := aParent;
  FIndex := aIndex;
  FFormLayout := aFormLayout;
  //
  FContainer := TLayout.Create(nil);
  FContainer.Parent := FParent;
  FContainer.Position.X := FParent.Width + 10;
  FContainer.Position.Y := 0;
  FContainer.Height := FParent.Height;
  FContainer.Width := FParent.Width;
  FContainer.ClipChildren := True;
  // embedd screen
  if FFormLayout<>nil then
  begin
    FFormLayout.Parent := FContainer;
    FFormLayout.Align := TAlignLayout.alClient;
  end;
end;

destructor TLSNAction.Destroy;
begin
  FContainer.Free;
end;

procedure TLSNAction.HideScreen;
begin
  //FContainer.AnimateFloatDelay('Scale.X',0.8,0.2,0,TAnimationType.atIn,TInterpolationType.itLinear);
  //FContainer.AnimateFloatDelay('Scale.Y',0.8,0.2,0,TAnimationType.atIn,TInterpolationType.itLinear);
  FContainer.AnimateFloatDelay('Opacity',0,0.2,0,TAnimationType.In,TInterpolationType.Linear);
  FContainer.Position.X := FParent.Width+10;
  //FContainer.AnimateFloatDelay('Position.X',FParent.Width+10,0.5,0.2,TAnimationType.atIn,TInterpolationType.itQuartic);
end;

procedure TLSNAction.ShowScreen(aAnimationType: Integer);
begin
  FContainer.Height := FParent.Height;
  FContainer.Width := FParent.Width;
  FContainer.BringToFront;
  FContainer.Scale.X := 1;
  FContainer.Scale.Y := 1;
  case aAnimationType of
    1:
    begin
      FContainer.Opacity := 1;
      FContainer.Position.X := FParent.Width + 10;
      FContainer.AnimateFloatDelay('Position.X',0,0.3,0,TAnimationType.In,TInterpolationType.Quartic);
    end;
    else
    begin
      FContainer.Opacity := 0;
      FContainer.Position.X := 0;
      FContainer.AnimateFloatDelay('Opacity',1,0.2,0,TAnimationType.In,TInterpolationType.Linear);
    end;
  end;
  //FContainer.Position.X := FParent.Width + 10;
  //
  //FContainer.AnimateFloatDelay('Position.X',0,0.4,0.1,TAnimationType.atIn,TInterpolationType.itQuartic);
end;

procedure TLSNAction.Resize;
begin
  FContainer.Height := FParent.Height;
  FContainer.Width := FParent.Width;
  if FContainer.Position.X > 0 then
    FContainer.Position.X := FParent.Width + 10;
end;

{$ENDREGION}

{$REGION 'TLSNActionList'}

constructor TLSNActionList.Create;
begin
  FActionList := TList.Create;
  //
  FActiveAction := nil;
end;

destructor TLSNActionList.Destroy;
var
  lAction: TLSNAction;
  i: integer;
begin
  for i := 0 to FActionList.Count-1 do
  begin
    lAction := FActionList[i];
    lAction.Free;
  end;
  //
  FActionList.Free;
end;

procedure TLSNActionList.Add(aIndex: integer; aParent: TControl; aFormLayout: TLayout);
var
  lAction: TLSNAction;
begin
  lAction := TLSNAction.Create(aIndex, aParent, aFormLayout);
  FActionList.Add(lAction);
end;

function TLSNActionList.SearchAction(aIndex: integer): TLSNAction;
var
  lAction: TLSNAction;
  i: integer;
begin
  result := nil;
  for i := 0 to FActionList.Count-1 do
  begin
    lAction := FActionList[i];
    if lAction.Index = aIndex then
    begin
      result := lAction;
      exit;
    end;
  end;
end;

procedure TLSNActionList.OpenScreen(aIndex: integer; aAnimationType: Integer);
var
  lAction: TLSNAction;
begin
  if FActiveAction<>nil then
  begin
    if FActiveAction.Index=aIndex then exit;
    // hide current screen
    FActiveAction.HideScreen;
  end;
  //
  lAction := SearchAction(aIndex);
  if lAction<>nil then
  begin
    lAction.ShowScreen(aAnimationType);
    FActiveAction := lAction;
  end;
end;

procedure TLSNActionList.Resize;
var
  lAction: TLSNAction;
  i: integer;
begin
  for i := 0 to FActionList.Count-1 do
  begin
    lAction := FActionList[i];
    lAction.Resize;
  end;
end;

{$ENDREGION}



end.
