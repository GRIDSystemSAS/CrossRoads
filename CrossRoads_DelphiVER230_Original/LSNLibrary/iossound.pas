unit iossound;
 
(*  adapted from https://forums.embarcadero.com/thread.jspa?threadID=68910&stqc=true *)
 
{$IFDEF FPC}
  {$mode objfpc}{$H+}
  {$modeswitch objectivec1}
  {$linkframework AVFoundation}
{$ENDIF}
 
interface
 
uses
  SysUtils{$IFDEF FPC},iPhoneAll , FMX_Platform_iOS ,NSHelpers{$ENDIF};
 
Const optSound : boolean = True;
Function PlaySound( sndFileName : String; Wait: boolean=false) : boolean;
 
implementation
 
{$IFDEF FPC}
 
type 
  AVAudioPlayer = objcclass external(NSObject)
  public
    function initWithContentsOfURL_error(url: NSURL; outError: NSErrorPointer): id; message 'initWithContentsOfURL:error:';
    function play: Boolean; message 'play';
    function volume:double; message 'volume';
    procedure setvolume(newValue:single); message 'setvolume:';
    function isPlaying: Boolean; message 'isPlaying';
    function prepareToPlay: Boolean; message 'prepareToPlay';
    function duration:double; message 'duration';
  end;
  AVAudioSession= objcclass external(NSObject)
    public
    class function sharedInstance:id; message 'sharedInstance';
    function  setActive(beActive:boolean;outError:NSError):Boolean;message 'setActive:error:';
    function setCategory(theCategory:NSString;outError:NSError):boolean; message 'setCategory:error:';
  end;
{$ENDIF}
 
{$IFDEF FPC}
(*
this was moved from PlaySound
I still dont understand the garbage collection on iOS
snd.autorelease; seemed to be the proper way to do it
but it was causing some sound to not be played.
*)
const  snd : AVAudioPlayer=Nil;
{$ENDIF}
 
Function PlaySound( sndFileName : String; Wait: boolean=false) : boolean;
{$IFDEF FPC}
var
    audioSession :AVAudioSession;
    //snd : AVAudioPlayer;
    err : NSError;
    url : NSURL;
    temp : NSString;
{$ENDIF}
begin
  result := true;
  if not optSound then exit;
  
  {$IFDEF FPC}
  try
    AnsiStrToNSStr( sndFileName , temp );
    url := NSURL.fileURLWithPath(temp);
 
    audioSession:=AVAudioSession.sharedInstance;
    audioSession.setActive(true,err);
    audioSession.setCategory(nsstr(PChar('AVAudioSessionCategoryPlayback')),nil);
    if snd<>Nil then
      snd.release;
 
    snd := AVAudioPlayer.alloc.initWithContentsOfURL_error(url, @err);
    if not snd.prepareToPlay then
    Begin
      //snd.release;
      result := false;
      exit;
    end;
    //snd.autorelease;
    //snd.setvolume(0.9);
    if snd.play=false then
      result := false;
    { DONE -oiOS 0.64 -cBug : Double/take/drop need to wait a bit to allow the sound to be played compeltly. }
    if Wait then
      Sleep(500);
 
  except
    result := false;
  end;
  {$ENDIF}
end;
 
end.