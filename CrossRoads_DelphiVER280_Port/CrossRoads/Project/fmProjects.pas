unit fmProjects;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Filter.Effects,
  FMX.Objects, FMX.Ani, FMX.Layouts, fmCurrentProject, fmProjectElem, fmFolder,
  fmSprintCal, FMX.Effects, unGlobal, fmBurnDown, IOUtils;

type
  TfrmProjects = class(TForm)
    layBody: TLayout;
    layBodyLeft: TLayout;
    layProjects: TLayout;
    AnimLayoutShow1: TFloatAnimation;
    imgCurrentFolder: TImage;
    imgAddProject: TImage;
    MonochromeEffect1: TMonochromeEffect;
    layCurrentProject: TLayout;
    layOtherProjects: TLayout;
    layTop: TLayout;
    txtFolderName: TText;
    layOtherFolders: TLayout;
    rectAddFolder: TRectangle;
    imgAddFolder: TImage;
    MonochromeEffect2: TMonochromeEffect;
    layFolders: TLayout;
    AnimLayoutShow2: TFloatAnimation;
    layBodyBody: TLayout;
    layMenu: TLayout;
    laySprint: TLayout;
    AnimLayoutShow3: TFloatAnimation;
    layMenuLink: TLayout;
    imgMenuTargets: TImage;
    MonochromeEffect3: TMonochromeEffect;
    layMenuTop: TLayout;
    imgScrumbaord: TImage;
    MonochromeEffect4: TMonochromeEffect;
    imgResume: TImage;
    MonochromeEffect5: TMonochromeEffect;
    layPending: TLayout;
    layDetails: TLayout;
    AnimLayoutShow4: TFloatAnimation;
    AnimLayoutShow5: TFloatAnimation;
    imgPending: TImage;
    layPreviously: TLayout;
    imgPreviously: TImage;
    pnlLog: TScrollBox;
    imgFond: TImage;
    layLeft: TLayout;
    imgArchiveProjectClick: TImage;
    imgDeleteFolder: TImage;
    MonochromeEffect6: TMonochromeEffect;
    FloatAnimation3: TFloatAnimation;
    imgComming: TImage;
    layComing: TLayout;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure imgAddProjectClick(Sender: TObject);
    procedure imgAddFolderClick(Sender: TObject);
    procedure imgMenuTargetsClick(Sender: TObject);
    procedure imgScrumbaordClick(Sender: TObject);
    procedure imgResumeClick(Sender: TObject);
    procedure imgAddProjectMouseEnter(Sender: TObject);
    procedure imgAddProjectMouseLeave(Sender: TObject);
    procedure imgArchiveProjectClickClick(Sender: TObject);
    procedure imgDeleteFolderMouseEnter(Sender: TObject);
    procedure imgDeleteFolderMouseLeave(Sender: TObject);
    procedure imgDeleteFolderClick(Sender: TObject);
    procedure imgAddFolderMouseEnter(Sender: TObject);
    procedure imgAddFolderMouseLeave(Sender: TObject);
  private
    FPendingList: TTaskList;
    FfrmCurrentProject: TfrmCurrentProject;
    FfrmProjectList: TList;
    FfrmFolderList: TList;
    FfrmSprint1: TfrmSprintCal;
    FfrmPendingList: TList;
    FfrmLogList: TList;
    FfrmComingList: TList;
    FComingList: TTaskList;
    procedure InitPending;
    procedure InitLogs;
    procedure InitComing;
  public
    procedure AnimateScreen;
    procedure Initialize;
    procedure InitializeProjects;
    procedure InitializeFolders;
    procedure LoadProjects;
  end;

var
  frmProjects: TfrmProjects;

implementation

uses fmProjectModify, fmWelcomeMain, unStructure, fmFoldertModify,
fmTaskPostIt, unFunctions, dmTypeList, fmLogElement;

{$R *.fmx}

procedure TfrmProjects.FormCreate(Sender: TObject);
var
  i: integer;
  lfrmProject: TfrmProjectElem;
  lfrmFolder: TfrmFolder;
  lfrmTaskPostIt: TfrmTaskPostIt;
  lfrmLogElement: TfrmLogElement;
begin
  FPendingList := TTaskList.Create;
  FComingList := TTaskList.Create;
  //
  layProjects.Opacity := 0;
  layTop.Opacity := 0;
  layMenu.Opacity := 0;
  layDetails.Opacity := 0;
  layBodyBody.Opacity := 0;
  // Folders
  layFolders.Width := CST_Max_FoldersToShow*(CST_FolderSize+10);
  FfrmFolderList := TList.Create;
  for i := 0 to CST_Max_FoldersToShow-1 do
  begin
    lfrmFolder := TfrmFolder.Create(self);
    lfrmFolder.layBody.Parent := layFolders;
    lfrmFolder.Initialize;
    lfrmFolder.layBody.Position.X := i*(CST_FolderSize+10);
    FfrmFolderList.Add(lfrmFolder);
  end;
  //
  for i := 0 to CST_Max_FoldersToShow-1 do
  begin
    lfrmFolder := FfrmFolderList[i];
    lfrmFolder.layBody.Align := TAlignLayout.alLeft;
  end;
  // current project
  FfrmCurrentProject := TfrmCurrentProject.Create(self);
  FfrmCurrentProject.layBody.Parent := layCurrentProject;
  FfrmCurrentProject.OnArchiveClick := imgArchiveProjectClickClick;
  // Projects
  FfrmProjectList := TList.Create;
  for i := 0 to CST_Max_ProjectsToShow-1 do
  begin
    lfrmProject := TfrmProjectElem.Create(self);
    lfrmProject.layBody.Parent := layOtherProjects;
    lfrmProject.Initialize;
    FfrmProjectList.Add(lfrmProject);
  end;
  //
  for i := CST_Max_ProjectsToShow-1 downto 0 do
  begin
    lfrmProject := FfrmProjectList[i];
    lfrmProject.layBody.Align := TAlignLayout.alTop;
  end;
  //
  FfrmSprint1 := TfrmSprintCal.Create(self);
  FfrmSprint1.layBody.Parent := laySprint;
  // Pending tasks
  FfrmPendingList := TList.Create;
  for i := 0 to CST_Max_PendingTasksinWelcome-1 do
  begin
    lfrmTaskPostIt := TfrmTaskPostIt.Create(nil);
    lfrmTaskPostIt.layBody.Parent := layPending;
    lfrmTaskPostIt.layBody.Position.Y := i*50;
    lfrmTaskPostIt.Initialize(CST_TargetPendingInWelcomePage);
    FfrmPendingList.Add(lfrmTaskPostIt);
  end;
  //
  for i := 0 to CST_Max_PendingTasksinWelcome-1 do
  begin
    lfrmTaskPostIt := FfrmPendingList[i];
    lfrmTaskPostIt.layBody.Align := TAlignLayout.alTop;
  end;
  // Coming tasks
  FfrmComingList := TList.Create;
  for i := 0 to CST_Max_ComingTasksinWelcome-1 do
  begin
    lfrmTaskPostIt := TfrmTaskPostIt.Create(nil);
    lfrmTaskPostIt.layBody.Parent := layComing;
    lfrmTaskPostIt.layBody.Position.Y := i*50;
    lfrmTaskPostIt.Initialize(CST_TargetPendingInWelcomePage);
    FfrmComingList.Add(lfrmTaskPostIt);
  end;
  //
  for i := 0 to CST_Max_ComingTasksinWelcome-1 do
  begin
    lfrmTaskPostIt := FfrmComingList[i];
    lfrmTaskPostIt.layBody.Align := TAlignLayout.alTop;
  end;
  // comments
  FfrmLogList := TList.Create;
  for i := 0 to CST_Max_LogsToShow-1 do
  begin
    lfrmLogElement := TfrmLogElement.Create(nil);
    lfrmLogElement.layBody.Parent := pnlLog;
    lfrmLogElement.Initialize;
    lfrmLogElement.layBody.Position.X := 4;
    lfrmLogElement.layBody.Position.Y := (i*CST_LogHeight)+4;
    FfrmLogList.Add(lfrmLogElement);
  end;
end;

procedure TfrmProjects.FormDestroy(Sender: TObject);
var
  lfrmProject: TfrmProjectElem;
  lfrmFolder: TfrmFolder;
  i: integer;
  lfrmTaskPostIt: TfrmTaskPostIt;
  lfrmLogElement: TfrmLogElement;
begin
  FPendingList.Free;
  //
  for i := 0 to FfrmPendingList.Count-1 do
  begin
    lfrmTaskPostIt := FfrmPendingList[i];
    lfrmTaskPostIt.Free;
  end;
  FfrmPendingList.Clear;
  //
  FComingList.Free;
  //
  for i := 0 to FfrmComingList.Count-1 do
  begin
    lfrmTaskPostIt := FfrmComingList[i];
    lfrmTaskPostIt.Free;
  end;
  FfrmComingList.Clear;
  //
  for i := 0 to FfrmFolderList.Count-1 do
  begin
    lfrmFolder := FfrmFolderList[i];
    lfrmFolder.Free;
  end;
  FfrmFolderList.Clear;
  //
  for i := 0 to FfrmProjectList.Count-1 do
  begin
    lfrmProject := FfrmProjectList[i];
    lfrmProject.Free;
  end;
  FfrmProjectList.Clear;
  //
  for i := 0 to FfrmLogList.Count-1 do
  begin
    lfrmLogElement := FfrmLogList[i];
    lfrmLogElement.Free;
  end;
  FfrmLogList.Clear;
  FfrmLogList.Free;
  //
  FfrmCurrentProject.Free;
  FfrmProjectList.Free;
  FfrmFolderList.Free;
  //
  FfrmSprint1.Free;
end;

procedure TfrmProjects.imgScrumbaordClick(Sender: TObject);
begin
  frmWelcomeMain.LaunchScrumboardMenu;
end;

procedure TfrmProjects.imgAddFolderClick(Sender: TObject);
var
  lFolder: string;
begin
  lFolder := '';
  if frmFolderModify.Show(lFolder) then
  begin
    frmWelcomeMain.InitializeFolder(lFolder);
  end;
end;

procedure TfrmProjects.imgAddFolderMouseEnter(Sender: TObject);
begin
  frmWelcomeMain.ShowHint('Add a new folder...');
end;

procedure TfrmProjects.imgAddFolderMouseLeave(Sender: TObject);
begin
  frmWelcomeMain.HideHint;
end;

procedure TfrmProjects.imgAddProjectClick(Sender: TObject);
var
  lProjectNum: integer;
  lProject: TProject;
begin
  lProjectNum := MyParams.ParamInteger[prmNextProjectNum];
  lProject := TProject.Create;
  lProject.Initialize(lProjectNum);
  //
  if frmProjectModify.Show(lProject.Data) then
  begin
    frmWelcomeMain.InitializeCurrentProject(lProjectNum);
    //
    MyParams.ParamInteger[prmNextProjectNum] := lProjectNum+1;
    LoadProjects;
  end;
end;

procedure TfrmProjects.imgAddProjectMouseEnter(Sender: TObject);
begin
  frmWelcomeMain.ShowHint('Add a new project...');
end;

procedure TfrmProjects.imgAddProjectMouseLeave(Sender: TObject);
begin
  frmWelcomeMain.HideHint;
end;

procedure TfrmProjects.imgArchiveProjectClickClick(Sender: TObject);
var
  lProject: TProject;
  lProjectRec: TProjectRecord;
  lProjectNum: integer;
begin
  if MessageDlg('Archive project ?', TMsgDlgType.mtConfirmation, mbYesNo, 0)=mrNo then exit;
  //
  lProject := GlobalVar.CurrentProject;
  lProjectRec := lProject.Data;
  lProjectRec.Status := staArchived;
  MainDatamodule.UpdateProject(lProjectRec);
  lProject := GlobalVar.ProjectList.Items(0);
  if lProject.Data.ProjectNum=GlobalVar.CurrentProjectNum then
  begin
    if GlobalVar.ProjectList.Count>1 then
      lProject := GlobalVar.ProjectList.Items(0)
    else
      lProject := nil;
  end;
  if lProject <> nil then
    lProjectNum := lProject.Data.ProjectNum
  else
    lProjectNum := -1;
  GlobalVar.ProjectList.Load;
  frmWelcomeMain.InitializeCurrentProject(lProjectNum);
end;

procedure TfrmProjects.imgDeleteFolderClick(Sender: TObject);
var
  lParamStr, lFolder: string;
  lFolderNb, i, lFolderPos: integer;
begin
  if MessageDlg('Delete folder ?', TMsgDlgType.mtConfirmation, mbYesNo, 0)=mrNo then exit;
  //
  lParamStr := Format(prmFolderDirectory,[GlobalVar.SelectedFolder]);
  MyFolders.ParamString[lParamStr] := '';
  // find folder position
  lFolderNb := MyFolders.ParamInteger[prmNextFolderNum]-1;
  lFolderPos := -1;
  i := 0;
  while (lFolderPos = -1) do
  begin
    lFolder := MyFolders.ParamString[Format('F%d',[i+1])];
    if lFolder = GlobalVar.SelectedFolder then
      lFolderPos := i;
    inc(i);
  end;
  // reaffect all following positions
  for i := lFolderPos to lFolderNb-2 do
  begin
    MyFolders.ParamString[Format('F%d',[i+1])] := MyFolders.ParamString[Format('F%d',[i+2])];
  end;
  MyFolders.ParamString[Format('F%d',[lFolderNb])] := '';
  MyFolders.ParamInteger[prmNextFolderNum] := MyFolders.ParamInteger[prmNextFolderNum]-1;
  MyFolders.Save;
  frmWelcomeMain.InitializeFolder(MyFolders.ParamString['F1']);
end;

procedure TfrmProjects.imgDeleteFolderMouseEnter(Sender: TObject);
begin
  frmWelcomeMain.ShowHint('Delete a folder...');
end;

procedure TfrmProjects.imgDeleteFolderMouseLeave(Sender: TObject);
begin
  frmWelcomeMain.HideHint;
end;

procedure TfrmProjects.imgMenuTargetsClick(Sender: TObject);
begin
  frmWelcomeMain.LaunchTargetsMenu;
end;

procedure TfrmProjects.imgResumeClick(Sender: TObject);
begin
  frmWelcomeMain.LaunchResumeMenu;
end;

procedure TfrmProjects.AnimateScreen;
begin
  AnimLayoutShow1.Enabled := True;
  AnimLayoutShow2.Enabled := True;
  AnimLayoutShow5.Enabled := True;
  AnimLayoutShow3.Enabled := True;
  AnimLayoutShow4.Enabled := True;
end;

procedure TfrmProjects.Initialize;
begin
  InitializeFolders;
  //
  LoadProjects;
  InitializeProjects;
  //
  FfrmSprint1.Initialize(date);
end;

procedure TfrmProjects.LoadProjects;
var
  lProject: TProject;
  lIndexForm: Integer;
  lDelay: Single;
  lfrmProject: TfrmProjectElem;
  i: Integer;
begin
  // initialize projects
  for i := 0 to CST_Max_ProjectsToShow - 1 do
  begin
    lfrmProject := FfrmProjectList[i];
    lfrmProject.Initialize;
  end;
  //
  lIndexForm := 0;
  for i := 0 to GlobalVar.ProjectList.Count - 1 do
  begin
    if lIndexForm >= CST_Max_ProjectsToShow then
      break;
    //
    lProject := GlobalVar.ProjectList.Items(i);
//    if (lProject.Data.ProjectNum <> GlobalVar.CurrentProjectNum)
//      and (lProject.Data.Status <> staArchived) then
    if (lProject.Data.Status <> staArchived) then
    begin
      lfrmProject := FfrmProjectList[lIndexForm];
      lDelay := CST_TransitionDelay * (i + 1);
      lfrmProject.Initialize(lProject.Data, lDelay);
      inc(lIndexForm);
    end;
  end;
  //
  imgAddProject.Visible := (GlobalVar.ProjectList.Count<CST_Max_ProjectsToShow);
end;

procedure TfrmProjects.InitializeProjects;
begin
  FfrmCurrentProject.Initialize(GlobalVar.CurrentProject.Data);
  InitPending;
  InitComing;
  InitLogs;
  //
  imgDeleteFolder.Visible := (GlobalVar.CurrentProject.TargetCount=0)
                              and (GlobalVar.ProjectList.Count<=1)
                              and (MyFolders.ParamInteger[prmNextFolderNum]>2);
end;

procedure TfrmProjects.InitializeFolders;
var
  lIndexForm: Integer;
  lDelay: Single;
  lfrmFolder: TfrmFolder;
  i, lFolderNb: Integer;
  lFolder: string;
begin
  txtFolderName.Text := UpperCase(GlobalVar.SelectedFolder);
  // initialize
  for i := 0 to CST_Max_FoldersToShow - 1 do
  begin
    lfrmFolder := FfrmFolderList[i];
    lfrmFolder.Initialize;
  end;
  //
  lIndexForm := 0;
  lFolderNb := MyFolders.ParamInteger[prmNextFolderNum]-1;
  for i := lFolderNb-1 downto 0 do
  begin
    if lIndexForm >= CST_Max_FoldersToShow then
      break;
    //
    lFolder := MyFolders.ParamString[Format('F%d',[i+1])];
    if lFolder <> GlobalVar.SelectedFolder then
    begin
      lfrmFolder := FfrmFolderList[lIndexForm];
      lDelay := CST_TransitionDelay * (i + 1);
      lfrmFolder.Initialize(lFolder, lDelay);
      inc(lIndexForm);
    end;
  end;
  //
  imgAddFolder.Visible := (lFolderNb<CST_Max_FoldersToShow);
  layFolders.Visible := (lFolderNb>1);
  layFolders.Width := CST_FolderSize*(lFolderNb-1);
end;

procedure TfrmProjects.InitPending;
var
  lfrmTaskPostIt: TfrmTaskPostIt;
  i: Integer;
  lTask: TTask;
  lDelay: Single;
  lIndexForm: Integer;
  lSprintCode: string;
begin
  FPendingList.Load(GlobalVar.CurrentProjectNum, CST_TargetScheduled, 0, GlobalVar.CurrentSprintCode);
  // initialize pending
  for i := 0 to CST_Max_PendingTasksinWelcome - 1 do
  begin
    lfrmTaskPostIt := FfrmPendingList[i];
    lfrmTaskPostIt.Initialize(CST_TargetPendingInWelcomePage);
  end;
  //
  lIndexForm := 0;
  for i := 0 to FPendingList.Count - 1 do
  begin
    if lIndexForm >= CST_Max_PendingTasksinWelcome then
      break;
    //
    lTask := FPendingList.Items(i);
    lSprintCode := GetSprintCode(date);
    if (lTask.Data.SprintCode = lSprintCode)
       and ((lTask.Data.Status = staScheduled) or (lTask.Data.Status = staInProgress)) then
    begin
      lfrmTaskPostIt := FfrmPendingList[lIndexForm];
      lDelay := CST_TransitionDelay * (i + 1);
      lfrmTaskPostIt.Initialize(lTask, lDelay);
      inc(lIndexForm);
    end;
  end;
end;

procedure TfrmProjects.InitComing;
var
  lfrmTaskPostIt: TfrmTaskPostIt;
  i: Integer;
  lTask: TTask;
  lDelay: Single;
  lIndexForm: Integer;
  lDateComing: TDatetime;
begin
  FComingList.Load(GlobalVar.CurrentProjectNum, CST_TargetToCome, 0);
  // initialize coming soon
  for i := 0 to CST_Max_ComingTasksinWelcome - 1 do
  begin
    lfrmTaskPostIt := FfrmComingList[i];
    lfrmTaskPostIt.Initialize(CST_TargetPendingInWelcomePage);
  end;
  //
  lIndexForm := 0;
  for i := 0 to FComingList.Count - 1 do
  begin
    if lIndexForm >= CST_Max_ComingTasksinWelcome then
      break;
    //
    lTask := FComingList.Items(i);
    lDateComing := Date+7;
    if (lTask.Data.Status = staValidated)
      and (lTask.Data.DueDate <> 0)
      and (lTask.Data.DueDate <= lDateComing) then
    begin
      lfrmTaskPostIt := FfrmComingList[lIndexForm];
      lDelay := CST_TransitionDelay * (i + 1);
      lfrmTaskPostIt.Initialize(lTask, lDelay);
      inc(lIndexForm);
    end;
  end;
end;

procedure TfrmProjects.InitLogs;
var
  i: integer;
  lfrmLog: TfrmLogElement;
  lLogRec: TUserLogRecord;
  lDelay: Single;
  lIndexForm: Integer;
begin
  // initialize logs
  for i := 0 to CST_Max_LogsToShow - 1 do
  begin
    lfrmLog := FfrmLogList[i];
    lfrmLog.Initialize;
  end;
  //
  lIndexForm := 0;
  i := 0;
  UserLogDatamodule.cdsUserLog.First;
  while not UserLogDatamodule.cdsUserLog.Eof do
  begin
    if lIndexForm >= CST_Max_LogsToShow then
      break;
    //
    UserLogDatamodule.GetUserLog(UserLogDatamodule.cdsUserLog.FieldByName('LogID').AsString, lLogRec);
    lfrmLog := FfrmLogList[lIndexForm];
    lDelay := CST_TransitionDelay * (i + 1);
    lfrmLog.Initialize(lLogRec, lDelay);
    inc(lIndexForm);
    inc(i);
    //
    UserLogDatamodule.cdsUserLog.Next;
  end;
  //
end;


end.
