//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
// unLSNSound v.1.0
// by Laurent Sengmany - 05.08.2012
//-----------------------------------------------------------------------------
// Description : this unit contains several classes and functions for sounds.
// -- NSHelpers should be used to your XCode project
// -- Sounds should be place in your XCode project resources (root)
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
unit unLSNSound;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
  {$modeswitch objectivec1}
{$ENDIF}

interface

{$IFDEF FPC}
  {$linkframework AVFoundation}
{$ENDIF}

uses
  SysUtils, Classes, Types
  {$IFDEF FPC}, iPhoneAll, FMX_Platform_iOS, NSHelpers{$ENDIF};

type
  {$IFDEF FPC}
  AVAudioPlayer = objcclass external(NSObject)
  public
    function initWithContentsOfURL_error(url: NSURL; outError: NSErrorPointer): id; message 'initWithContentsOfURL:error:';
    function play: Boolean; message 'play';
    function volume:double; message 'volume';
    procedure setvolume(newValue:single); message 'setvolume:';
    function isPlaying: Boolean; message 'isPlaying';
    function prepareToPlay: Boolean; message 'prepareToPlay';
    function duration:double; message 'duration';
    procedure pause; message 'pause';
    procedure stop; message 'stop';
    procedure setNumberOfLoops (newValue: NSInteger); message 'setNumberOfLoops:';
    function numberOfLoops: NSInteger; message 'numberOfLoops';
  end;

  AVAudioSession= objcclass external(NSObject)
    public
    class function sharedInstance:id; message 'sharedInstance';
    function  setActive(beActive:boolean;outError:NSError):Boolean; message 'setActive:error:';
    function setCategory(theCategory:NSString;outError:NSError):boolean; message 'setCategory:error:';
  end;
  {$ENDIF}

  TLSNSound = class
  private
    // variables and procedures for iOS
    {$IFDEF FPC}
    FAudioSession: AVAudioSession;
    FAVAudioPlayer: AVAudioPlayer;
    function ioInitialize(aFilename: string): Boolean;
    {$ENDIF}
    function GetIsActive: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function Initialize(aFilename: string): Boolean;
    function Play: Boolean;
    procedure Pause;
    procedure Stop;
    procedure EnableLoop;
    procedure DisableLoop;
    //
    property IsActive: Boolean read GetIsActive;
  end;

implementation


{$REGION 'TLSNSound'}

constructor TLSNSound.Create;
begin
  {$IFDEF FPC}
  FAVAudioPlayer := nil;
  {$ENDIF}
end;

destructor TLSNSound.Destroy;
begin
  {$IFDEF FPC}
  if FAVAudioPlayer<>nil then
    FAVAudioPlayer.Release;
  {$ENDIF}
  inherited;
end;

procedure TLSNSound.EnableLoop;
begin
  {$IFDEF FPC}
  if FAVAudioPlayer<>nil then
    FAVAudioPlayer.setNumberOfLoops(-1);
  {$ENDIF}
end;

function TLSNSound.GetIsActive: Boolean;
begin
  result := False;
  {$IFDEF FPC}
  result := (FAVAudioPlayer<>nil);
  {$ENDIF}
end;

procedure TLSNSound.DisableLoop;
begin
  {$IFDEF FPC}
  if FAVAudioPlayer<>nil then
    FAVAudioPlayer.setNumberOfLoops(0);
  {$ENDIF}
end;

function TLSNSound.Initialize(aFilename: string): Boolean;
begin
  result := False;
  {$IFDEF FPC}
  result := ioInitialize(aFilename);
  {$ENDIF}
end;

procedure TLSNSound.Pause;
begin
  {$IFDEF FPC}
  if FAVAudioPlayer<>nil then
    FAVAudioPlayer.pause;
  {$ENDIF}
end;

function TLSNSound.Play: Boolean;
begin
  result := False;
  {$IFDEF FPC}
  if (FAVAudioPlayer<>nil) then
    result := FAVAudioPlayer.play;
  {$ENDIF}
end;

procedure TLSNSound.Stop;
begin
  {$IFDEF FPC}
  if (FAVAudioPlayer<>nil) then
    FAVAudioPlayer.stop;
  {$ENDIF}
end;

// -----------------------------------------------------------------
// Procedures for iOS/FPC
// -----------------------------------------------------------------
{$IFDEF FPC}

function TLSNSound.ioInitialize(aFilename: string): Boolean;
var
  lErr: NSError;
  lUrl: NSURL;
  lPath: NSString;
begin
  result := False;
  //
  FAudioSession := AVAudioSession.sharedInstance;
  FAudioSession.setActive(true, lErr);
  FAudioSession.setCategory(nsstr(PChar('AVAudioSessionCategoryPlayback')),nil);
  //
  lPath := NSBundle.mainBundle.resourcePath.stringByAppendingPathComponent(AnsiStrToNSStr(aFilename));
  lUrl := NSURL.fileURLWithPath(lPath);
  //
  if FAVAudioPlayer<>nil then
    FAVAudioPlayer.Release;
  FAVAudioPlayer := AVAudioPlayer.alloc.initWithContentsOfURL_error(lUrl, @lErr);
  if not FAVAudioPlayer.prepareToPlay then
  Begin
    FAVAudioPlayer.release;
    FAVAudioPlayer := nil;
    exit;
  end;
  result := True;
end;

{$ENDIF}
// -----------------------------------------------------------------

{$ENDREGION}

end.
