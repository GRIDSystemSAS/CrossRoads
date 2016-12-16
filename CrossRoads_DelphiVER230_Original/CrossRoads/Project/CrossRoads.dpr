program CrossRoads;

uses
  MidasLib,
  FMX.Forms,
  unGlobal in 'unGlobal.pas',
  dmProjectList in 'dmProjectList.pas' {dtmProjectList: TDataModule},
  unStructure in 'unStructure.pas',
  fmProjectElem in 'fmProjectElem.pas' {frmProjectElem},
  fmTargetModify in 'fmTargetModify.pas' {frmTargetModify},
  unFunctions in 'unFunctions.pas',
  fmCurrentProject in 'fmCurrentProject.pas' {frmCurrentProject},
  dmTypeList in 'dmTypeList.pas' {dtmTypeList: TDataModule},
  fmWelcomeMain in 'fmWelcomeMain.pas' {frmWelcomeMain},
  fmProjects in 'fmProjects.pas' {frmProjects},
  fmTasks in 'fmTasks.pas' {frmTasks},
  fmTaskReport in 'fmTaskReport.pas' {frmTaskReport},
  fmFoldertModify in 'fmFoldertModify.pas' {frmFolderModify},
  fmTargetTodo in 'fmTargetTodo.pas' {frmTargetTodo},
  fmScrumBoard in 'fmScrumBoard.pas' {frmScrumBoard},
  fmSprint in 'fmSprint.pas' {frmSprint},
  dmTaskList in 'dmTaskList.pas' {dtmTaskList: TDataModule},
  fmTarget in 'fmTarget.pas' {frmTarget},
  fmTaskPostIt in 'fmTaskPostIt.pas' {frmTaskPostIt},
  fmFolder in 'fmFolder.pas' {frmFolder},
  fmProjectModify in 'fmProjectModify.pas' {frmProjectModify},
  fmLinkElement in 'fmLinkElement.pas' {frmLinkElement},
  fmLoading in 'fmLoading.pas' {frmLoading},
  fmSprintCal in 'fmSprintCal.pas' {frmSprintCal},
  fmCommentElement in 'fmCommentElement.pas' {frmCommentElement},
  fmTaskElement in 'fmTaskElement.pas' {frmTaskElement},
  dmUserLog in 'dmUserLog.pas' {dtmUserLog: TDataModule},
  fmLogElement in 'fmLogElement.pas' {frmLogElement},
  fmBurnUp in 'fmBurnUp.pas' {frmBurnUp},
  fmVelocity in 'fmVelocity.pas' {frmVelocity},
  fmHint in 'fmHint.pas' {frmHint},
  fmSearch in 'fmSearch.pas' {frmSearch},
  fmTaskPending in 'fmTaskPending.pas' {frmTaskPending},
  fmAbout in 'fmAbout.pas' {frmAbout},
  dmLSNParameters in '..\..\LSNLibrary\dmLSNParameters.pas' {dtmLSNParameters: TDataModule},
  unLSNMenu in '..\..\LSNLibrary\unLSNMenu.pas',
  unLSNSyncDB in '..\..\LSNLibrary\unLSNSyncDB.pas',
  unLSNUtils in '..\..\LSNLibrary\unLSNUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmWelcomeMain, frmWelcomeMain);
  Application.CreateForm(TfrmTargetModify, frmTargetModify);
  Application.CreateForm(TfrmFolderModify, frmFolderModify);
  Application.CreateForm(TfrmProjectModify, frmProjectModify);
  Application.CreateForm(TfrmAbout, frmAbout);
  Application.CreateForm(TdtmLSNParameters, dtmLSNParameters);
  Application.Run;
end.
