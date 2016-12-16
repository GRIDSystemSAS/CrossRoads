unit fmWelcomeMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects,
  unGlobal, FMX.Layouts, FMX.Ani, fmProjects, unLSNMenu, fmTasks,
  dmTypeList, FMX.Filter.Effects, fmTargetTodo, fmScrumBoard, IOUtils, fmResume,
  fmHint, FMX.Platform, fmSearch, FMX.Effects;

type
  TfrmWelcomeMain = class(TForm)
    pnlBodyMain: TRectangle;
    pnlBody: TRectangle;
    layTop: TLayout;
    imgBandeau: TImage;
    AnimBandeau: TFloatAnimation;
    pnlHideQuote: TRectangle;
    AnimQuote: TFloatAnimation;
    imgBandLeft: TImage;
    AnimBandLeft: TFloatAnimation;
    layBody: TLayout;
    pnlProjectName: TRectangle;
    layBandeauRight: TLayout;
    lblProjectName: TText;
    AnimProjectName: TFloatAnimation;
    imgMenuProject: TImage;
    AnimMonoProject: TMonochromeEffect;
    layMenuIcon: TLayout;
    AnimMenu: TFloatAnimation;
    imgMenuTarget: TImage;
    AnimMonoTarget: TMonochromeEffect;
    imgMenuBoard: TImage;
    AnimMonoBoard: TMonochromeEffect;
    rectFolder: TRectangle;
    lblFolderName: TText;
    layBottom: TLayout;
    imgBack: TImage;
    SepiaEffect1: TSepiaEffect;
    imgResume: TImage;
    AnimMonoResume: TMonochromeEffect;
    TimerSave: TTimer;
    layLoading: TLayout;
    layHint: TLayout;
    rectBottom: TRectangle;
    img2C: TImage;
    rectVersion: TRectangle;
    txtVersion: TText;
    rectDir: TRectangle;
    txtDirectory: TText;
    imgMenuTargetTodo: TImage;
    AnimMonoTargetTodo: TMonochromeEffect;
    layAbout: TLayout;
    imgAbout: TImage;
    MonochromeEffect1: TMonochromeEffect;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure imgMenuProjectClick(Sender: TObject);
    procedure imgMenuTargetClick(Sender: TObject);
    procedure imgMenuBoardClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure imgBackClick(Sender: TObject);
    procedure TimerSaveTimer(Sender: TObject);
    procedure imgResumeClick(Sender: TObject);
    procedure imgAboutClick(Sender: TObject);
  private
    FMenuLayoutList: TList;
    FActionList: TLSNActionList;
    FActiveMenu: TControl;
    FActiveAnimMono: TMonochromeEffect;
    FPrecMenu: integer;
    FEnvLoaded: Boolean;
    procedure CreateEnvironment;
    procedure AnimateScreen;
    procedure CreateMenu;
    procedure OpenScreen(aControl: TControl; aMonochromeAnim: TMonochromeEffect);
    procedure AnimateMenus;
  public
    procedure InitializeCurrentProject(aProjectNum: integer);
    procedure InitializeFolder(aFolder: string);
    procedure ShowTarget(aTask: TTask);
    procedure ShowPrecMenu;
    procedure SaveEnvironment;
    procedure LaunchTargetsMenu;
    procedure LaunchScrumboardMenu;
    procedure LaunchResumeMenu;
    procedure ShowHint(aHint: string);
    procedure HideHint;
    procedure UpdateStats;
  end;

var
  frmWelcomeMain: TfrmWelcomeMain;

implementation

uses unLSNUtils, dmLSNParameters, dmProjectList, unStructure, dmTaskList,
unFunctions, fmLoading, dmUserLog, fmAbout;

{$R *.fmx}



procedure TfrmWelcomeMain.FormActivate(Sender: TObject);
begin
  if FEnvLoaded then exit;
  FEnvLoaded := True;
  //
  frmLoading := TfrmLoading.Create(self);
  frmLoading.rectBody.Parent := layLoading;
  frmLoading.Initialize;
  //
  frmHint := TfrmHint.Create(self);
  frmHint.layBody.Parent := layHint;
  frmHint.Initialize;
  //
  frmLoading.ShowProgress('Loading menus...',5);
  CreateMenu;
  OnResize := FormResize;
  //
  GlobalVar := TGlobalVariable.Create;
  CreateEnvironment;
  //
  frmLoading.ShowProgress('Loading current project...',90);
  frmProjects.Initialize;
  //
  frmLoading.ShowProgress('End of loading...',100);
  layLoading.Visible := False;
  //
  AnimateScreen;
end;

procedure TfrmWelcomeMain.FormCreate(Sender: TObject);
begin
  OnResize := nil;
  //
  Width := 1200;
  Height := 800;
  //
  imgBandeau.Position.X := 2000;
  imgBandLeft.Position.X := -100;
  layBandeauRight.Opacity := 0;
  layMenuIcon.Opacity := 0;
  //
  FEnvLoaded := False;
  //
  txtVersion.Text := Format('Release V%s',[CST_ApplicationVersion]);
  txtDirectory.Text := '';
end;

procedure TfrmWelcomeMain.CreateMenu;
begin
  // create menu
  FActionList := TLSNActionList.Create;
  //
  frmLoading.ShowProgress('Creating Projects menu...',10);
  frmProjects := TfrmProjects.Create(Self);
  imgMenuProject.Tag := CST_Menu_Projects;
  FActionList.Add(CST_Menu_Projects, layBody, frmProjects.layBody);
  OpenScreen(imgMenuProject, animMonoProject);
  //
  frmLoading.ShowProgress('Creating Tasks menu...',15);
  frmTasks := TfrmTasks.Create(Self);
  imgMenuTarget.Tag := CST_Menu_Tasks;
  FActionList.Add(CST_Menu_Tasks, layBody, frmTasks.layBody);
  //
  frmLoading.ShowProgress('Creating Targets menu...',20);
  frmTargetTodo := TfrmTargetTodo.Create(Self);
  imgMenuTargetTodo.Tag := CST_Menu_TargetToDo;
  FActionList.Add(CST_Menu_TargetToDo, layBody, frmTargetTodo.layBody);
  //
  frmLoading.ShowProgress('Creating Scrumboard menu...',30);
  frmScrumBoard := TfrmScrumBoard.Create(Self);
  imgMenuBoard.Tag := CST_Menu_ScrumBoard;
  FActionList.Add(CST_Menu_ScrumBoard, layBody, frmScrumBoard.layBody);
  //
  frmLoading.ShowProgress('Creating Resume menu...',40);
  frmSearch := TfrmSearch.Create(Self);
  imgResume.Tag := CST_Menu_Resume;
  FActionList.Add(CST_Menu_Resume, layBody, frmSearch.layBody);
end;


procedure TfrmWelcomeMain.FormDestroy(Sender: TObject);
begin
  SaveEnvironment;
  //
  MyPath.Free;
  MyParams.Free;
  MyFolders.Free;
  FMenuLayoutList.Free;
  MainDatamodule.Free;
  TaskDatamodule.Free;
  TypeDatamodule.Free;
  MyGlobalParams.Free;
  UserLogDatamodule.Free;
end;

procedure TfrmWelcomeMain.FormResize(Sender: TObject);
begin
  FActionList.Resize;
end;

procedure TfrmWelcomeMain.HideHint;
begin
  frmHint.HideHint;
  layHint.SendToBack;
end;

procedure TfrmWelcomeMain.imgAboutClick(Sender: TObject);
begin
  frmAbout.Initialize;
  frmAbout.ShowModal;
end;

procedure TfrmWelcomeMain.imgBackClick(Sender: TObject);
begin
  imgMenuProjectClick(Sender);
end;

procedure TfrmWelcomeMain.imgMenuBoardClick(Sender: TObject);
begin
  LaunchScrumboardMenu;
end;

procedure TfrmWelcomeMain.imgMenuProjectClick(Sender: TObject);
begin
  frmProjects.Initialize;
  FPrecMenu := imgMenuProject.Tag;
  OpenScreen(imgMenuProject, animMonoProject);
  imgBack.Visible := False;
end;

procedure TfrmWelcomeMain.imgMenuTargetClick(Sender: TObject);
begin
  LaunchTargetsMenu;
end;

procedure TfrmWelcomeMain.imgResumeClick(Sender: TObject);
begin
  LaunchResumeMenu;
end;

procedure TfrmWelcomeMain.ShowHint(aHint: string);
var
  p : TPointF;
  MouseService: IFMXMouseService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXMouseService, IInterface(MouseService)) then
  begin
    p := MouseService.GetMousePos;
    layHint.Position.X := p.X - Left - layBody.Position.X -10;
    layHint.Position.Y := p.Y - Top - layBody.Position.Y -50;
    layHint.BringToFront;
    frmHint.ShowHint(aHint);
  end;
end;

procedure TfrmWelcomeMain.ShowPrecMenu;
begin
  case FPrecMenu of
    CST_Menu_Projects : imgMenuProjectClick(imgMenuProject);
    CST_Menu_Tasks : imgMenuTargetClick(imgMenuTarget);
    CST_Menu_ScrumBoard : imgMenuBoardClick(imgMenuBoard);
    CST_Menu_Resume : imgResumeClick(imgResume);
  end;
end;



procedure TfrmWelcomeMain.ShowTarget(aTask: TTask);
begin
  frmTargetTodo.Initialize(aTask);
  OpenScreen(imgMenuTargetTodo, animMonoTargetTodo);
end;

procedure TfrmWelcomeMain.TimerSaveTimer(Sender: TObject);
begin
  SaveEnvironment;
end;

procedure TfrmWelcomeMain.SaveEnvironment;
begin
  //pnlBody.Enabled := False;
  try
    UpdateStats;
    //
    MyFolders.Save;
    MyParams.Save;
    MainDatamodule.Save;
    TaskDatamodule.Save;
    TypeDatamodule.Save;
    MyGlobalParams.Save;
    UserLogDatamodule.Save;
  except
    //
  end;
  //pnlBody.Enabled := True;
end;

procedure TfrmWelcomeMain.UpdateStats;
var
  lTaskList: TTaskList;
  lNbScheduled, lNbUnclassified, lNbDone: Integer;
  lNbNew, lNbNewUnclassified, lNbInProgress: Integer;
  lSumScheduled, lSumUnclassified, lSumDone: Integer;
  lSumNew, lSumNewUnclassified, lSumInProgress: Integer;
  lTask: TTask;
  i: integer;
  lStatsRec: TStatsRecord;
  lSprintRec: TSprintRecord;
begin
  TypeDatamodule.GetSprint(GlobalVar.StatsRecord.SprintCode, lSprintRec);
  lTaskList := TTaskList.Create;
  lTaskList.Load(GlobalVar.CurrentProjectNum, CST_TargetScheduled, 0, GlobalVar.StatsRecord.SprintCode);
  //
  lNbScheduled := 0;
  lNbUnclassified := 0;
  lNbDone := 0;
  lNbNew := 0;
  lNbNewUnclassified := 0;
  lNbInProgress := 0;
  lSumScheduled := 0;
  lSumUnclassified := 0;
  lSumDone := 0;
  lSumNew := 0;
  lSumNewUnclassified := 0;
  lSumInProgress := 0;
  //
  for i := 0 to lTaskList.Count-1 do
  begin
    lTask := lTaskList.Items(i);
    inc(lNbScheduled);
    lSumScheduled := lSumScheduled + lTask.Data.Estimation;
    if (lTask.Data.ParentTaskNum=CST_UnclassifiedTodo) then
    begin
      inc(lNbUnclassified);
      lSumUnclassified := lSumUnclassified + lTask.Data.Estimation;
    end;
    if (lTask.Data.Status=staDone) then
    begin
      inc(lNbDone);
      lSumDone := lSumDone + lTask.Data.Estimation;
    end;
    if (lTask.Data.Status=staInProgress) then
    begin
      inc(lNbInProgress);
      lSumInProgress := lSumInProgress + lTask.Data.Estimation;
    end;
    if (lTask.Data.CreationDate>=lSprintRec.BeginDate)
        and (lTask.Data.CreationDate<=lSprintRec.EndDate) then
    begin
      inc(lNbNew);
      lSumNew := lSumNew + lTask.Data.Estimation;
    end;
    if (lTask.Data.CreationDate>=lSprintRec.BeginDate)
        and (lTask.Data.CreationDate<=lSprintRec.EndDate)
        and (lTask.Data.ParentTaskNum=CST_UnclassifiedTodo) then
    begin
      inc(lNbNewUnclassified);
      lSumNewUnclassified := lSumNewUnclassified + lTask.Data.Estimation;
    end;
  end;
  //
  lStatsRec.StatDate := GlobalVar.StatsRecord.StatDate;
  lStatsRec.SprintCode := GlobalVar.StatsRecord.SprintCode;
  lStatsRec.NbScheduled := lNbScheduled;
  lStatsRec.NbUnclassified := lNbUnclassified;
  lStatsRec.NbDone := lNbDone;
  lStatsRec.NbInProgress := lNbInProgress;
  lStatsRec.NbNew := lNbNew;
  lStatsRec.NbNewUnclassified := lNbNewUnclassified;
  lStatsRec.SumScheduled := lSumScheduled;
  lStatsRec.SumUnclassified := lSumUnclassified;
  lStatsRec.SumDone := lSumDone;
  lStatsRec.SumInProgress := lSumInProgress;
  lStatsRec.SumNew := lSumNew;
  lStatsRec.SumNewUnclassified := lSumNewUnclassified;
  GlobalVar.StatsRecord := lStatsRec;
  //
  TaskDataModule.UpdateStats(lStatsRec);
end;

procedure TfrmWelcomeMain.LaunchTargetsMenu;
begin
  frmTasks.Initialize;
  FPrecMenu := imgMenuTarget.Tag;
  OpenScreen(imgMenuTarget, animMonoTarget);
  imgBack.Visible := True;
end;

procedure TfrmWelcomeMain.LaunchScrumboardMenu;
begin
  frmScrumBoard.Initialize;
  FPrecMenu := imgMenuBoard.Tag;
  OpenScreen(imgMenuBoard, animMonoBoard);
  imgBack.Visible := True;
end;

procedure TfrmWelcomeMain.LaunchResumeMenu;
begin
  frmSearch.Initialize;
  FPrecMenu := imgResume.Tag;
  OpenScreen(imgResume, animMonoResume);
  imgBack.Visible := True;
end;


procedure TfrmWelcomeMain.CreateEnvironment;
var
  lParamFile: string;
begin
  MyPath := TLSNPath.Create(CST_ApplicationName);
  // load parameters
  frmLoading.ShowProgress('Loading parameters...',50);
  MyGlobalParams := TdtmLSNParameters.Create(self);
  lParamFile := Format('%s.cfg',[CST_ApplicationName]);
  MyGlobalParams.Initialize(MyPath.ParameterDirectory, lParamFile);
  MyGlobalParams.Load;
  //
  MyGlobalParams.ParamString[prmApplicationVersion] := CST_ApplicationVersion;
  // check for directories registered in parameters...
  if MyGlobalParams.ParamString[prmMainDataDirectory]='' then
    MyGlobalParams.ParamString[prmMainDataDirectory] := MyPath.DataDirectory;
  GlobalVar.MainDataDirectory := MyGlobalParams.ParamString[prmMainDataDirectory];
  //
  if MyGlobalParams.ParamString[prmSelectedFolder]='' then
  begin
    MyGlobalParams.ParamString[prmSelectedFolder] := 'Projects';
  end;
  // init folder
  InitializeFolder(MyGlobalParams.ParamString[prmSelectedFolder]);
  //
  MyGlobalParams.Save;
end;

procedure TfrmWelcomeMain.InitializeFolder(aFolder: string);
var
  lProjectNum: Integer;
  lParamStr: string;
begin
  GlobalVar.CurrentSprintCode := GetSprintCode(date);
  GlobalVar.SelectedFolder := aFolder;
  MyGlobalParams.ParamString[prmSelectedFolder] := aFolder;
  lblFolderName.Text := UpperCase(aFolder);
  // load folders
  frmLoading.ShowProgress('Loading folders...',55);
  MyFolders := TdtmLSNParameters.Create(self);
  MyFolders.Initialize(MyPath.ParameterDirectory, 'Folders.cfg');
  MyFolders.Load;
  //
  if MyFolders.ParamInteger[prmNextFolderNum] = 0 then
    MyFolders.ParamInteger[prmNextFolderNum] := 1;
  // check for data directory
  lParamStr := Format(prmFolderDirectory,[GlobalVar.SelectedFolder]);
  if MyFolders.ParamString[lParamStr]='' then
  begin
    MyFolders.ParamString[lParamStr] := GlobalVar.MainDataDirectory + TPath.DirectorySeparatorChar + GlobalVar.SelectedFolder;
    MyFolders.ParamString[Format('F%d',[MyFolders.ParamInteger[prmNextFolderNum]])] := GlobalVar.SelectedFolder;
    MyFolders.ParamInteger[prmNextFolderNum] := MyFolders.ParamInteger[prmNextFolderNum]+1;
  end;
  GlobalVar.DataDirectory := MyFolders.ParamString[lParamStr];
  txtDirectory.Text := GlobalVar.DataDirectory;
  MyFolders.Save;
  frmProjects.InitializeFolders;
  //
  MyParams := TdtmLSNParameters.Create(self);
  MyParams.Initialize(GlobalVar.DataDirectory, 'Parameters.cfg');
  MyParams.Load;
  // set default parameters
  if MyParams.ParamInteger[prmWeekCapacity] = 0 then
  begin
    MyParams.ParamInteger[prmWeekCapacity] := 40;
  end;
  GlobalVar.WeekCapacity := MyParams.ParamInteger[prmWeekCapacity];
  // load types
  if TypeDatamodule<>nil then
    TypeDatamodule.Free;
  TypeDatamodule := TdtmTypeList.Create(self);
  TypeDatamodule.Initialize(GlobalVar.DataDirectory);
  TypeDatamodule.Load;
  TypeDatamodule.CheckSprintParameters;
  // load projects
  frmLoading.ShowProgress('Loading data...',60);
  if MainDatamodule<>nil then
    MainDatamodule.Free;
  MainDatamodule := TdtmProjectList.Create(self);
  MainDatamodule.Initialize(GlobalVar.DataDirectory);
  MainDatamodule.Load;
  // create TaskDatamodule
  if TaskDatamodule<>nil then
    TaskDatamodule.Free;
  TaskDatamodule := TdtmTaskList.Create(self);
  // create UserLogDatamodule
  if UserLogDatamodule<>nil then
    UserLogDatamodule.Free;
  UserLogDatamodule := TdtmUserLog.Create(self);
  //
  frmLoading.ShowProgress('Init global variables...',65);
  GlobalVar.Initialize;
  // set current project
  lProjectNum := MyParams.ParamInteger[prmCurrentProjectNum];
  if lProjectNum = 0 then
  begin
    inc(lProjectNum);
    MyParams.ParamInteger[prmCurrentProjectNum] := lProjectNum;
    MyParams.ParamInteger[prmNextProjectNum] := lProjectNum + 1;
    MyParams.ParamInteger[prmNextTaskNum] := 1;
  end;
  InitializeCurrentProject(lProjectNum);
end;

procedure TfrmWelcomeMain.InitializeCurrentProject(aProjectNum: integer);
begin
  frmLoading.ShowProgress('Init current project...',70);
  GlobalVar.CurrentProjectNum := aProjectNum;
  MyParams.ParamInteger[prmCurrentProjectNum] := aProjectNum;
  //
  lblProjectName.Text := UpperCase(GlobalVar.CurrentProject.Data.ProjectName);
  //
  frmLoading.ShowProgress('Init frmProjects...',75);
  try
    frmProjects.InitializeProjects;
  except
    on E:Exception do
    begin
      Showmessage(E.Message);
    end;
  end;
  frmLoading.ShowProgress('Update stats...',80);
  try
    UpdateStats;
  except
    on E:Exception do
    begin
      Showmessage(E.Message);
    end;
  end;
end;

procedure TfrmWelcomeMain.AnimateScreen;
begin
  AnimBandeau.Enabled := True;
  AnimQuote.Enabled := True;
  AnimBandLeft.Enabled := True;
  AnimProjectName.Enabled := True;
  AnimMenu.Enabled := True;
  AnimMenu.Enabled := True;
  //
  frmProjects.AnimateScreen;
end;

procedure TfrmWelcomeMain.OpenScreen(aControl: TControl; aMonochromeAnim: TMonochromeEffect);
begin
  if FActiveMenu<>nil then
    FActiveMenu.HitTest := True;
  FActionList.OpenScreen(aControl.Tag);
  FActiveMenu := aControl;
  FActiveAnimMono := aMonochromeAnim;
  FActiveMenu.HitTest := False;
  AnimateMenus;
end;

procedure TfrmWelcomeMain.AnimateMenus;
begin
  animMonoProject.Enabled := True;
  animMonoTarget.Enabled := True;
  animMonoBoard.Enabled := True;
  AnimMonoResume.Enabled := True;
  if FActiveMenu<>nil then
    FActiveAnimMono.Enabled := False;
end;



end.
