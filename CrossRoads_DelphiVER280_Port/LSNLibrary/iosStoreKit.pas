Unit iosStoreKit;
(*  Store Kit in Delphi
 
    By Olecramoak
    Last version date: 1.10 - July 1st 2012
 
    usage:
      Call GetAppIdentifierData( 'com.yoursite.yourapp.yourinapppurchase' ); //
      Display the price, name and description  ( GlobalVariable UpgradeTitle, UpgradeDescription, UpgradeMyValue )
      To purchase call BuyItem with a callback that will process the results
 
    example:
--------------------------------------------------------------------------------------------------------
 
    Procedure Purchase(aStatus: integer; Identifier: string; aErrorCode: integer; aMessage: string);
    var R: integer;
    begin
      try
        if assigned(PurchaseDlg) then
        Begin
          PurchaseDlg.AniIndicator1.Visible := false;
          PurchaseDlg.AniIndicator1.Enabled := false;
        end;
      except
      end;
 
      { DONE -oiOS 0.71 -cHelp : Prompt to rate }
      case aStatus of
        SKPaymentTransactionStatePurchasing: Begin end;
        SKPaymentTransactionStateRestored: Begin
          TrialLevel := tlPremium;
          MainForm.SaveSettings;
          XGMessage ( 'Your purchase has been restored, thank you!', xgmOk, false,false );
        end;
        SKPaymentTransactionStatePurchased: Begin
          TrialLevel := tlPremium;
          MainForm.SaveSettings;
          R := XGMessage ( 'Thank you for your purchase!'+#13+'Do you want to rate XG Mobile on the App store?', xgmYesno, false,false );
        end;
        SKPaymentTransactionStateFailed: Begin
          case aErrorCode of
            SKErrorUnknown           : XGMessage ( aMessage, xgmOk, false,false );
            SKErrorClientInvalid     : XGMessage ( 'Invalid client', xgmOk, false,false );
            SKErrorPaymentCancelled  : Begin end; // do not display anyting as user cancelled.
            SKErrorPaymentInvalid    : XGMessage ( 'Invalid Payment', xgmOk, false,false );
            SKErrorPaymentNotAllowed : XGMessage ( 'Payment not allowed', xgmOk, false,false );
          end;
        end;
        //non standard ios error code
        SKPaymentTransactionStateCannotPurchase: Begin
          XGMessage ( 'In-App purchases are not avaliable at this time.'+#13+'Verify they are enabled on your device in '+
                      'Settings>General>Restrictions', xgmOk, false,false,220 );
        end;
        SKPaymentTransactionNothingToRrestore : Begin
          XGMessage ( 'You do not have any purchases to restore', xgmOk, false,false );
        end;
        else Begin
          XGMessage ( 'Unknown Error', xgmOk, false,false );
        end;
      end;
      if assigned(PurchaseDlg) then
      Begin
        PurchaseDlg.rtpremium.Enabled := true;
        PurchaseDlg.rtRestore.Enabled := true;
      end;
    end;
 
    Const cancel : boolean = false;
 
    procedure TPurchaseDlg.Button1Click(Sender: TObject);
    begin
      //Cancel is a global set to tryue to aborth the process
      If GetAppIdentifierData(AppPremiumString, cancel ) then   //AppPremiumString='com.mydomain.myapp.mypremium'
      Begin
        //okay got data
        teprice.Text   := UpgradeMyValue;
        teprice.Font.Size := 24;
        //teupgrade.Text := UpgradeTitle;
        //teDescription.Text := UpgradeDescription;
        rtPremium.Enabled := true;
        rtupgrade.Visible := true;
        rtrestore.Visible := true;
      end else
      Begin
        AniIndicator1.Visible := false;
        AniIndicator1.Enabled := false;
        if not cancel then
          XGMessage ( 'Cannot connect to app store', xgmOk, false,false );
      end;
    end;
 
    procedure TPurchaseDlg.rtPremiumClick(Sender: TObject);
    begin
      if rtpremium.Enabled = false then exit;
      rtpremium.Enabled := false;
      BuyItem(Purchase);
    end;
--------------------------------------------------------------------------------------------------------
 
    history:
    1.10: implement Restoreitem  see Olecramoak email (7/1/2012)
          Added the IAP indenfier to the callback
          Added SKPaymentTransactionNothingToRrestore to error status list for the call back
    1.00: initial version
 
 
*)
 
{$IFDEF FPC}
{$mode delphi}
{$modeswitch objectivec2}
{$ENDIF}
 
Interface
 
uses SysUtils, Classes, FMX_Forms,FMX_Types{$IFDEF FPC}, iPhoneAll,FMX_Platform_iOS {$ENDIF};
 
type TBuyCallBack = Procedure(aStatus: integer; Identifier: string; aErrorCode: integer; aMessage: string);
 
type
  TPurchaseThread = class(TThread)
  Public
    procedure Execute; override;
  end;
  TRestoreThread = class(TThread)
  Public
    procedure Execute; override;
  end;
 
 
const
  SKPaymentTransactionStatePurchasing = 0;
  SKPaymentTransactionStatePurchased = 1;
  SKPaymentTransactionStateFailed = 2;
  SKPaymentTransactionStateRestored = 3;
  //XDC non standard ios error code
  SKPaymentTransactionStateCannotPurchase = 100;
  SKPaymentTransactionUnknownError = 101;
  SKPaymentTransactionNothingToRrestore = 102;
 
const
  SKErrorNone = -1;  //XDC not IOS standard
  SKErrorUnknown = 0;
  SKErrorClientInvalid = 1;
  SKErrorPaymentCancelled = 2;
  SKErrorPaymentInvalid = 3;
  SKErrorPaymentNotAllowed = 4;
 
{$IFDEF FPC}
 
{$linkframework StoreKit}
 
type
  SKPaymentTransactionState = NSInteger;
  SKPaymentTransactionStatePtr = ^SKPaymentTransactionState;
 
 
Type
 
{ SKProduct }
  SKProduct = objcclass external (NSObject)
  private
    _internal: id;
 
  public
    function localizedDescription: NSString; message 'localizedDescription';
    function localizedTitle: NSString; message 'localizedTitle';
    function price: NSDecimalNumber; message 'price';
    function priceLocale: NSLocale; message 'priceLocale';
    function productIdentifier: NSString; message 'productIdentifier';
  end;
 
{ SKPayment }
  SKPayment = objcclass external (NSObject, NSCopyingProtocol, NSMutableCopyingProtocol)
  private
    _internal: id;
 
  public
    class function paymentWithProduct(product: SKProduct): id; message 'paymentWithProduct:';
    class function paymentWithProductIdentifier(identifier: NSString): id; message 'paymentWithProductIdentifier:';
    function productIdentifier: NSString; message 'productIdentifier';
    function requestData: NSData; message 'requestData';
    function quantity: NSInteger; message 'quantity';
 
    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
    function mutableCopyWithZone(zone_: NSZonePtr): id;
  end;
 
{ SKMutablePayment }
  SKMutablePayment = objcclass external (SKPayment)
 
  public
    procedure setProductIdentifier (newValue: NSString); message 'setProductIdentifier:';
    function productIdentifier: NSString; message 'productIdentifier';
    procedure setQuantity (newValue: NSInteger); message 'setQuantity:';
    function quantity: NSInteger; message 'quantity';
    procedure setRequestData (newValue: NSData); message 'setRequestData:';
    function requestData: NSData; message 'requestData';
  end;
 
{ SKPaymentTransaction }
  SKPaymentTransaction = objcclass external (NSObject)
  private
    _internal: id;
 
  public
    function error: NSError; message 'error';
    function originalTransaction: SKPaymentTransaction; message 'originalTransaction';
    function payment: SKPayment; message 'payment';
    function transactionDate: NSDate; message 'transactionDate';
    function transactionIdentifier: NSString; message 'transactionIdentifier';
    function transactionReceipt: NSData; message 'transactionReceipt';
    function transactionState: SKPaymentTransactionState; message 'transactionState';
  end;
 
  SKPaymentQueue  = objcclass;
 
  { SKPaymentTransactionObserver Protocol }
  SKPaymentTransactionObserverProtocol = objcprotocol external name 'SKPaymentTransactionObserver'
  required
    procedure paymentQueue_updatedTransactions(queue: SKPaymentQueue; transactions: NSArray); message 'paymentQueue:updatedTransactions:';
  optional
    procedure paymentQueue_removedTransactions(queue: SKPaymentQueue; transactions: NSArray); message 'paymentQueue:removedTransactions:';
    procedure paymentQueue_restoreCompletedTransactionsFailedWithError(queue: SKPaymentQueue; error: NSError); message 'paymentQueue:restoreCompletedTransactionsFailedWithError:';
    procedure paymentQueueRestoreCompletedTransactionsFinished(queue: SKPaymentQueue); message 'paymentQueueRestoreCompletedTransactionsFinished:';
  end;
 
{ SKPaymentQueue }
  SKPaymentQueue = objcclass external (NSObject)
  private
    _internal: id;
 
  public
    class function defaultQueue: SKPaymentQueue; message 'defaultQueue';
    class function canMakePayments: Boolean; message 'canMakePayments';
    procedure addPayment(payment: SKPayment); message 'addPayment:';
    procedure restoreCompletedTransactions; message 'restoreCompletedTransactions';
    procedure finishTransaction(transaction: SKPaymentTransaction); message 'finishTransaction:';
    procedure addTransactionObserver(observer: SKPaymentTransactionObserverProtocol); message 'addTransactionObserver:';
    procedure removeTransactionObserver(observer: SKPaymentTransactionObserverProtocol); message 'removeTransactionObserver:';
    function transactions: NSArray; message 'transactions';
  end;
 
 
  { SKRequest }
  SKRequest = objcclass external (NSObject)
  private
    _requestInternal: id;
 
  public
    procedure setDelegate (newValue: id); message 'setDelegate:';
    function delegate: id; message 'delegate';
    procedure cancel; message 'cancel';
    procedure start; message 'start';
  end;
 
  { SKRequestDelegate Protocol }
  SKRequestDelegateProtocol = objcprotocol external name 'SKRequestDelegate'
  optional
    procedure requestDidFinish(request: SKRequest); message 'requestDidFinish:';
    procedure request_didFailWithError(request: SKRequest; error: NSError); message 'request:didFailWithError:';
  end;
 
{ SKProductsRequest }
  SKProductsRequest = objcclass external (SKRequest)
  private
    _productsRequestInternal: id;
 
  public
    function initWithProductIdentifiers(productIdentifiers: NSSet): id; message 'initWithProductIdentifiers:';
    procedure setDelegate (newValue: id); message 'setDelegate:';
    function delegate: id; message 'delegate';
  end;
 
{ SKProductsResponse }
  SKProductsResponse = objcclass external (NSObject)
  private
    _internal: id;
 
  public
    function products: NSArray; message 'products';
    function invalidProductIdentifiers: NSArray; message 'invalidProductIdentifiers';
  end;
 
 
{ SKProductsRequestDelegate Protocol }
  SKProductsRequestDelegateProtocol = objcprotocol external name 'SKProductsRequestDelegate'
  required
    procedure productsRequest_didReceiveResponse(request: SKProductsRequest; response: SKProductsResponse); message 'productsRequest:didReceiveResponse:';
  end;
 
 
type
  TInAppPurchase = objcclass(NSObject)
    procedure requestProUpgradeProductData; message 'requestProUpgradeProductData';
  end;
 
type
  TInAppPurchaseDelegate = objcclass(NSObject, SKRequestDelegateProtocol)
    procedure requestDidFinish(request: SKRequest); message 'requestDidFinish:';
    procedure request_didFailWithError(request: SKRequest; error: NSError); message 'request:didFailWithError:';
    procedure productsRequest_didReceiveResponse(request: SKProductsRequest; response: SKProductsResponse); message 'productsRequest:didReceiveResponse:';
  end;
 
type
  TPaymentDelegate = objcclass(NSObject, SKPaymentTransactionObserverProtocol)
    procedure paymentQueue_updatedTransactions(queue: SKPaymentQueue;transactions: NSArray); message 'paymentQueue:updatedTransactions:';
    procedure paymentQueueRestoreCompletedTransactionsFinished( queue: SKPaymentQueue ) ;  message 'paymentQueueRestoreCompletedTransactionsFinished:' ;
    procedure paymentQueue_restoreCompletedTransactionsFailedWithError( queue: SKPaymentQueue ; error: NSError ) ; message 'paymentQueue:restoreCompletedTransactionsFailedWithError:' ;
  end;
 
var
  InAppPurchase: TInAppPurchase;
  InAppPurchaseDelegate: TInAppPurchaseDelegate;
  PaymentDelegate: TPaymentDelegate;
  PaymentQueue: SKPaymentQueue;
  Product: SKProduct;
  Payment: SKPayment;
  productsRequest: SKProductsRequest;
 
 
{$ENDIF}
var   AppIdentifier : string;
 
var   RequestEnded : boolean;
      FetchDataOk  : boolean;
      UpgradeTitle : String;
      UpgradeDescription : String;
      UpgradeMyValue   : String;
      UpgradeID        : String;
 
 
Function GetAppIdentifierData( aAppIdentifier : string; Var Cancel : Boolean ) : boolean;
procedure BuyItem(acallback: TBuyCallBack);
procedure RestoreItem(acallback: TBuyCallBack);
 
implementation
 
uses FMX_dialogs;
 
var BuyCallBack : TBuyCallBack;
 
procedure BuyItem(acallback: TBuyCallBack);
var MyPurchaseThread: TPurchaseThread;
begin
  //XDC add the callback
  buyCallBack := acallback;
//  MyPurchaseThread := TPurchaseThread.Create(True);
//  MyPurchaseThread.FreeOnTerminate := True;
//  MyPurchaseThread.Execute;
//  Application.ProcessMessages;
{$IFDEF FPC}
  PaymentDelegate := TPaymentDelegate.Alloc.Init;
  Payment := SKPayment.Alloc;
  Payment := SKPayment.paymentWithProductIdentifier(NSSTR(PCHAR(AppIdentifier)));
  PaymentQueue := SKPaymentQueue.Alloc.Init;
  if PaymentQueue.CanMakePayments then
  begin
    PaymentQueue.addTransactionObserver(PaymentDelegate);
    PaymentQueue.addPayment(Payment);
  end
  else
  begin
    buyCallBack(SKPaymentTransactionStateCannotPurchase,'',SKErrorNone,'');
  end;
{$ENDIF}
end;
 
procedure RestoreItem(acallback: TBuyCallBack);
var MyRestoreThread: TRestoreThread;
begin
  //XDC add the callback
  buyCallBack := acallback;
//  MyRestoreThread := TRestoreThread.Create(True);
//  MyRestoreThread.FreeOnTerminate := True;
//  MyRestoreThread.Execute;
//  Application.ProcessMessages;
{$IFDEF FPC}
  PaymentDelegate := TPaymentDelegate.Alloc.Init;
  PaymentQueue := SKPaymentQueue.Alloc.Init;
  if PaymentQueue.CanMakePayments then
  begin
    PaymentQueue.addTransactionObserver(PaymentDelegate);
    PaymentQueue.restoreCompletedTransactions;
  end
  else
  begin
    buyCallBack(SKPaymentTransactionStateCannotPurchase,'',SKErrorNone,'');
  end;
{$ENDIF}
end;
 
procedure TPurchaseThread.Execute;
begin
{$IFDEF FPC}
  PaymentDelegate := TPaymentDelegate.Alloc.Init;
  Payment := SKPayment.Alloc;
  Payment := SKPayment.paymentWithProductIdentifier(NSSTR(PCHAR(AppIdentifier)));
  PaymentQueue := SKPaymentQueue.Alloc.Init;
  if PaymentQueue.CanMakePayments then
  begin
    PaymentQueue.addTransactionObserver(PaymentDelegate);
    PaymentQueue.addPayment(Payment);
  end
  else
  begin
    buyCallBack(SKPaymentTransactionStateCannotPurchase,'',SKErrorNone,'');
  end;
{$ENDIF}
end;
 
procedure TRestoreThread.Execute;
begin
{$IFDEF FPC}
  PaymentDelegate := TPaymentDelegate.Alloc.Init;
  PaymentQueue := SKPaymentQueue.Alloc.Init;
  if PaymentQueue.CanMakePayments then
  begin
    PaymentQueue.addTransactionObserver(PaymentDelegate);
    PaymentQueue.restoreCompletedTransactions;
  end
  else
  begin
    buyCallBack(SKPaymentTransactionStateCannotPurchase,'',SKErrorNone,'');
  end;
{$ENDIF}
end;
 
{$IFDEF FPC}
 
PROCEDURE TPaymentDelegate.paymentQueue_updatedTransactions(queue: SKPaymentQueue; transactions: NSArray);
var
  transaction: SKPaymentTransaction;
  Nstr: NSString;
  error: NSError;
  MyPayment: SKPayment ;
  Identifier: string;
  I:integer;
begin
  transaction := SKPaymentTransaction.Alloc;
  if transactions.Count >= 1 then
    begin
      //for i := 0 to transactions.Count-1 do
      begin
        transaction := transactions.objectAtIndex(0);
        //Change log 1.10
        MyPayment := transaction.Payment;
        Nstr := MyPayment.productidentifier;
        Identifier := string( Nstr.UTF8String );
        //end change
        case transaction.transactionState of
          SKPaymentTransactionStatePurchased:
            begin
              PaymentQueue.finishTransaction(transaction);
              PaymentQueue.removeTransactionObserver(PaymentDelegate);
              buyCallBack(transaction.transactionState,Identifier,SKErrorNone,'');
              //PURCHASE OK
            end;
          SKPaymentTransactionStateRestored:
            begin
              PaymentQueue.finishTransaction(transaction);
              PaymentQueue.removeTransactionObserver(PaymentDelegate);
              buyCallBack(transaction.transactionState,Identifier,SKErrorNone,'');
              //PURCHASE restore
            end;
          SKPaymentTransactionStateFailed:
            begin
              PaymentQueue.finishTransaction(transaction);
              error := NSError.Alloc;
              error := transaction.error;
              Nstr := error.LocalizedDescription;
              PaymentQueue.removeTransactionObserver(PaymentDelegate);
              buyCallBack(transaction.transactionState,'',error.Code,String(Nstr.UTF8String));
            end;
          SKPaymentTransactionStatePurchasing:
            begin
              //('Transaction going on.');
            end;
        else
          begin
            //
          end;
        end; // CASE
      end;
    end
  else
    begin
      buyCallBack(SKPaymentTransactionUnknownError,'',SKErrorNone,'');
    end;
end;
 
//CHANGE LOG 1.1
procedure TPaymentDelegate.paymentQueue_restoreCompletedTransactionsFailedWithError( queue: SKPaymentQueue ; error: NSError ) ;
var Nstr: NSString ;
begin
  if queue = PaymentQueue then
  begin
    PaymentQueue.removeTransactionObserver ( PaymentDelegate ) ;
    Nstr := error.LocalizedDescription ;
    buyCallBack(SKPaymentTransactionStateFailed,'',SKErrorUnknown,string( Nstr.UTF8String ));
  end;
end;
 
//CHANGE LOG 1.1
procedure TPaymentDelegate.paymentQueueRestoreCompletedTransactionsFinished( queue: SKPaymentQueue ) ;
begin
  if queue = PaymentQueue then
  begin
    PaymentQueue.removeTransactionObserver ( PaymentDelegate ) ;
    if queue.transactions.Count=0 then
      buyCallBack(SKPaymentTransactionNothingToRrestore,'',SKErrorNone,'');
  end;
end;
 
 
PROCEDURE TInAppPurchase.requestProUpgradeProductData;
var productIdentifiers: NSSet;
begin
  productIdentifiers := NSSet.SetWithObject(NSSTR(PCHAR(AppIdentifier)));//@Anders: AppIdentifier: your IAP identifier. Must be set up on iTC even for tests
  productsRequest := SKProductsRequest.Alloc.initWithProductIdentifiers(productIdentifiers);
  productsRequest.SetDelegate(InAppPurchaseDelegate);
  productsRequest.Start;
end;
 
procedure TInAppPurchaseDelegate.requestDidFinish(request: SKRequest);
begin
  RequestEnded := True;
end;
 
procedure TInAppPurchaseDelegate.request_didFailWithError(request: SKRequest; error: NSError);
var Nstr: NSString;
begin
  FetchDataOk := FALSE;
  Nstr := error.LocalizedDescription;
end;
 
procedure TInAppPurchaseDelegate.productsRequest_didReceiveResponse(request: SKProductsRequest; response: SKProductsResponse);
var
  MyProducts, Invalids: NSArray;
  nstrTitle, nstrDescrption, nstrID, nstrValue: NSString;
  MyValue: NSDecimalNumber;
  frmt : NSNumberFormatter;
begin
  MyProducts := NSArray.Alloc;
  MyProducts := response.products;
  RequestEnded := true;
  if MyProducts.Count >= 1 then
  begin
    FetchDataOk := True;
    Product := MyProducts.objectAtIndex(0);
    nstrTitle := Product.LocalizedTitle;
    nstrDescrption := Product.LocalizedDescription;
    MyValue := Product.Price;
    //CHANGE LOG 1.00
    frmt := NSNumberFormatter.alloc.init;
    frmt.setFormatterBehavior(NSNumberFormatterBehavior10_4);
    frmt.setNumberStyle(NSNumberFormatterCurrencyStyle);
    frmt.setLocale( Product.priceLocale);
    nstrValue := frmt.stringFromNumber(product.price);
    //END CHANGE
 
    nstrID := Product.productIdentifier;
    upgradeTitle:= String(nstrTitle.UTF8String);
    UpgradeDescription := String(nstrDescrption.UTF8String);
    UpgradeMyValue := String(nstrValue.UTF8String);
    UpgradeID := String(nstrID.UTF8String);
  end
  else
  begin
    FetchDataOk := FALSE;
    Invalids := NSArray.Alloc;
    Invalids := response.invalidProductIdentifiers;
  end;
end;
{$ENDIF}
 
Function GetAppIdentifierData( aAppIdentifier : string; Var Cancel : Boolean ) : boolean;
begin
  try
    RequestEnded := false;
    FetchDataOk := True;
    AppIdentifier := aAppIdentifier;
 
    {$IFDEF FPC}
    Product := SKProduct.Alloc;
    InAppPurchaseDelegate := TInAppPurchaseDelegate.Alloc;
    InAppPurchase := TInAppPurchase.Alloc.Init;
    //this is not needed as the  requestProUpgradeProductData set the delegate
    //productsRequest.SetDelegate(InAppPurchaseDelegate);
    InAppPurchase.requestProUpgradeProductData;
    {$ENDIF}
 
    repeat
      Application.ProcessMessages;
    until RequestEnded or Cancel;
    if Cancel then
    Begin
      result := false;
    end else
      result := FetchDataOk;
  except
    result := false;
  end;
end;
 
end.