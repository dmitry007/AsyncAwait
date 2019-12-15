(*

  Copyright 2019 Dmitry Streblechenko (dmitry@dimastr.com)
  Licensed under MIT license

*)

unit AsyncAwait;

interface

uses
  Windows, Classes, SysUtils, ComObj,
  ActiveX, Winapi.Mtx,
  ctxtcall, ObjIdl;

type

  {$SCOPEDENUMS ON}
  TTaskStatus = (Canceled, Created, Faulted, RanToCompletion, Running,
                 WaitingForActivation, WaitingForChildrenToComplete, WaitingToRun);
  {$SCOPEDENUMS OFF}

  TTask = class;
  TTask<T> = class;
  //ITaskScheduler = interface;
  ITask  = interface;
  ITask<T>  = interface;
  ISynchronizationContext = interface;

  TaskProc = reference to procedure();
  TaskFunc<T>  = reference to function() : T;

  TaskProcContinue = reference to procedure(task : ITask);
  TaskProcContinue<T> = reference to procedure(task : ITask<T>; Value : T);

  CancellationProc = reference to procedure();

  ICancellationToken = interface(IUnknown)
  ['{46B7C425-9978-4EB7-97C7-4D51AC58DF8A}']
    procedure Register(proc : CancellationProc);
    function GetCanBeCanceled : boolean;
    function GetIsCancellationRequested : boolean;
    function GetWaitHandle : THandle;
    procedure Cancel;
    //properties
    property CanBeCanceled : boolean read GetCanBeCanceled;
    property IsCancellationRequested : boolean read GetIsCancellationRequested;
    property WaitHandle : THandle read GetWaitHandle;
  end;

  ITask = interface(IUnknown)
  ['{DA0AD6C4-B37F-4461-B054-E57F85305F09}']
    function ContinueWith(proc : TaskProcContinue):ITask;overload;
    function ContinueWith(proc : TaskProcContinue; SynchronizationContext : ISynchronizationContext):ITask;overload;
    function GetException : Exception;
    function GetCancellationToken : ICancellationToken;
    function GetStatus : TTaskStatus;
    function GetWaitHandle : THandle;
    procedure Wait;overload;
    function Wait(milliseconds : integer) : boolean;overload;
    procedure Start;
    function ConfigureAwait(continueOnCapturedContext : boolean) : ITask;overload;
    //properties
    property Exception : Exception read GetException;
    property CancellationToken : ICancellationToken read GetCancellationToken;
    property Status : TTaskStatus read GetStatus;
    property WaitHandle : THandle read GetWaitHandle;
  end;

  ITask<T> = interface(ITask)
  ['{20C06A9C-75F8-4674-A70D-A6418081D2EA}']
    function ContinueWith(proc : TaskProcContinue<T>) : ITask<T>;overload;
    function ContinueWith(proc : TaskProcContinue<T>; SynchronizationContext : ISynchronizationContext):ITask<T>;overload;
    function GetValue : T;
    procedure SetValue(Value : T);
    function ConfigureAwait(continueOnCapturedContext : boolean) : ITask<T>;overload;
    //properties
    property Value : T read GetValue write SetValue;
  end;

  ISynchronizationContext = interface
  ['{0871D95C-28CA-4B6D-AE1B-F07A0F3F28FB}']
    function GetContextCallback : IContextCallback;
    function GetThreadType : THDTYPE;
    property ContextCallback : IContextCallback read GetContextCallback;
    property ThreadType : THDTYPE read GetThreadType;
  end;

  TSynchronizationContext = class(TInterfacedObject, ISynchronizationContext)
  protected
    fContextCallback : IContextCallback;
    fThreadType : THDTYPE;
    constructor CreateNew;
  public
    class function Current : ISynchronizationContext;

    function GetThreadType : THDTYPE;
    function GetContextCallback : IContextCallback;
  end;

  ///disable
  // H2269 Overriding virtual method 'TTask.Destroy' has lower visibility (protected) than base class 'TInterfacedObject' (public)
  {$HINTS OFF}
  TTask = class(TInterfacedObject, ITask)
  private

    type
      TTaskThread = class(TThread)
      private
        fTask : TTask;
      protected
        procedure Execute;override;
      public
        constructor Create(task : TTask);
      end;

  private
    fStatus : TTaskStatus;
    fThread : TTaskThread;
    fException: Exception;
    fProc : TaskProc;
    fContinueWith : TaskProcContinue;
    fContinueOnCapturedContext : boolean;
    fCancellationToken : ICancellationToken;
    fWaitHandle : THandle;

    fSynchronizationContext : ISynchronizationContext;

    procedure ThreadProc;
  protected
    constructor CreateNew(proc : TaskProc;
                       continueWith : TaskProcContinue;
                       continueOnCapturedContext : boolean;
                       CancellationToken : ICancellationToken);overload;

    destructor Destroy;override;
    procedure DoExecute;virtual;
    procedure DoContinue;virtual;
    function NeedToContinue : boolean;virtual;
  public
    class function Async(proc : TaskProc;
                         continueWith : TaskProcContinue;
                         continueOnCapturedContext : boolean;
                         CancellationToken : ICancellationToken) : ITask;overload;
    class function Async(proc : TaskProc) : ITask; overload;
    class function Async(proc : TaskProc; continueWith : TaskProcContinue) : ITask; overload;
    class function Async(proc : TaskProc; continueWith : TaskProcContinue; continueOnCapturedContext : boolean) : ITask; overload;
    class function Async(proc : TaskProc; continueWith : TaskProcContinue; CancellationToken : ICancellationToken) : ITask; overload;

    class function Run(proc : TaskProc) : ITask; overload;
    class function Run(proc : TaskProc; CancellationToken : ICancellationToken) : ITask; overload;

    class function Create(proc : TaskProc;
                       continueWith : TaskProcContinue;
                       continueOnCapturedContext : boolean;
                       CancellationToken : ICancellationToken) : ITask; overload;
    class function Create(proc : TaskProc) : ITask; overload;
    class function Create(proc : TaskProc; continueWith : TaskProcContinue) : ITask; overload;
    class function Create(proc : TaskProc; continueWith : TaskProcContinue; continueOnCapturedContext : boolean) : ITask;overload;
    class function Create(proc : TaskProc; continueWith : TaskProcContinue; CancellationToken : ICancellationToken) : ITask;overload;

    function ContinueWith(proc : TaskProcContinue) : ITask;overload;
    function ContinueWith(proc : TaskProcContinue; SynchronizationContext : ISynchronizationContext):ITask;overload;
    function ConfigureAwait(continueOnCapturedContext : boolean) : ITask;overload;
    function GetException : Exception;
    function GetCancellationToken : ICancellationToken;
    function GetStatus : TTaskStatus;
    function GetWaitHandle : THandle;
    procedure Start;
    procedure Wait;overload;
    function Wait(milliseconds : integer) : boolean; overload;
    class procedure WhenAll(const tasks : array of ITask);
    class procedure WhenAny(const tasks : array of ITask);
  end;
  {$HINTS ON}

  TTask<T> = class(TTask, ITask<T>)
  private
    fValue : T;
    fFunc : TaskFunc<T>;
    fContinueWithT : TaskProcContinue<T>;
  protected
    constructor CreateNew(func : TaskFunc<T>;
                       continueWith : TaskProcContinue<T>;
                       continueOnCapturedContext : boolean;
                       CancellationToken : ICancellationToken);overload;
    procedure DoExecute;override;
    procedure DoContinue;override;
    function NeedToContinue : boolean;override;
  public
    class function Async(func : TaskFunc<T>;
                         continueWith : TaskProcContinue<T>;
                         continueOnCapturedContext : boolean;
                         CancellationToken : ICancellationToken) : ITask<T>;overload;
    class function Async(func : TaskFunc<T>) : ITask<T>; overload;
    class function Async(func : TaskFunc<T>; continueWith : TaskProcContinue<T>) : ITask<T>; overload;
    class function Async(func : TaskFunc<T>; continueWith : TaskProcContinue<T>; continueOnCapturedContext : boolean) : ITask<T>; overload;
    class function Async(func : TaskFunc<T>; continueWith : TaskProcContinue<T>; CancellationToken : ICancellationToken) : ITask<T>; overload;

    class function Create(func : TaskFunc<T>;
                       continueWith : TaskProcContinue<T>;
                       continueOnCapturedContext : boolean;
                       CancellationToken : ICancellationToken) : ITask<T>;overload;
    class function Create(func : TaskFunc<T>) : ITask<T>;overload;
    class function Create(func : TaskFunc<T>; continueWith : TaskProcContinue<T>) : ITask<T>;overload;
    class function Create(func : TaskFunc<T>; continueWith : TaskProcContinue<T>; continueOnCapturedContext : boolean) : ITask<T>;overload;
    class function Create(func : TaskFunc<T>; continueWith : TaskProcContinue<T>; CancellationToken : ICancellationToken) : ITask<T>;overload;

    function ContinueWith(proc : TaskProcContinue<T>) : ITask<T>;overload;
    function ContinueWith(proc : TaskProcContinue<T>; SynchronizationContext : ISynchronizationContext):ITask<T>;overload;

    function ConfigureAwait(continueOnCapturedContext : boolean) : ITask<T>;overload;
    function GetValue : T;
    procedure SetValue(Value : T);
  end;

implementation

const
  IID_IObjectContext: TGUID = '{51372AE0-CAE7-11CF-BE81-00AA00A2FA25}';

function CoGetObjectContext(const riid: TGUID; out ObjectContext: IUnknown): HRESULT; stdcall; external 'Ole32.dll';

{ TTask }

class function TTask.Async(proc: TaskProc; continueWith: TaskProcContinue;
  continueOnCapturedContext: boolean): ITask;
begin
  Result := Self.Async(proc, continueWith, continueOnCapturedContext, nil);
end;

class function TTask.Async(proc: TaskProc;
  continueWith: TaskProcContinue): ITask;
begin
  Result := Self.Async(proc, continueWith, true, nil);
end;

class function TTask.Async(proc: TaskProc): ITask;
begin
  Result := Self.Async(proc, nil, true, nil);
end;

class function TTask.Async(proc: TaskProc; continueWith: TaskProcContinue;
  CancellationToken: ICancellationToken): ITask;
begin
  Result := Self.Async(proc, nil, true, CancellationToken);
end;

class function TTask.Async(proc: TaskProc; continueWith: TaskProcContinue;
  continueOnCapturedContext: boolean;
  CancellationToken: ICancellationToken): ITask;
begin
  Result := TTask.Create(proc, continueWith, continueOnCapturedContext, CancellationToken);
  Result.Start;
end;

function TTask.ConfigureAwait(continueOnCapturedContext: boolean): ITask;
begin
  fContinueOnCapturedContext := continueOnCapturedContext;
  Result := Self;
end;

function TTask.ContinueWith(proc: TaskProcContinue; SynchronizationContext : ISynchronizationContext): ITask;
begin
  fContinueWith := proc;
  fSynchronizationContext := SynchronizationContext;
  Result := Self;
end;

class function TTask.Create(proc: TaskProc) : ITask;
begin
  Result := CreateNew(proc, nil, true, nil) as ITask;
end;

function TTask.ContinueWith(proc: TaskProcContinue) : ITask;
begin
  Result := ContinueWith(proc, nil);
end;

class function TTask.Create(proc: TaskProc; continueWith: TaskProcContinue) : ITask;
begin
  Result := CreateNew(proc, continueWith, true, nil) as ITask;
end;

class function TTask.Create(proc: TaskProc; continueWith: TaskProcContinue;
  continueOnCapturedContext: boolean) : ITask;
begin
  Result := CreateNew(proc, continueWith, continueOnCapturedContext, nil) as ITask;
end;

class function TTask.Create(proc: TaskProc; continueWith: TaskProcContinue;
  CancellationToken: ICancellationToken) : ITask;
begin
  Result := CreateNew(proc, continueWith, true, CancellationToken) as ITask;
end;

class function TTask.Create(proc: TaskProc; continueWith: TaskProcContinue;
  continueOnCapturedContext: boolean;
  CancellationToken: ICancellationToken): ITask;
begin
  Result := CreateNew(proc, continueWith, continueOnCapturedContext, CancellationToken) as ITask;
end;

constructor TTask.CreateNew(proc: TaskProc; continueWith: TaskProcContinue;
  continueOnCapturedContext: boolean; CancellationToken: ICancellationToken);
begin
  inherited Create;
  fProc := proc;
  fContinueWith := continueWith;
  fContinueOnCapturedContext := continueOnCapturedContext;
  fCancellationToken := CancellationToken;
  fWaitHandle := CreateEventW(nil, true, false, nil);
  fStatus := TTaskStatus.Created;
end;

destructor TTask.Destroy;
begin
  CloseHandle(fWaitHandle);
  inherited;
end;

procedure TTask.DoContinue;
begin
  if Assigned(fContinueWith) then fContinueWith(Self);
end;

procedure TTask.DoExecute;
begin
  if Assigned(fProc) then fProc();
end;

function TTask.GetCancellationToken: ICancellationToken;
begin
  Result := fCancellationToken;
end;

function TTask.GetException: Exception;
begin
  Result := fException;
end;

function TTask.GetStatus: TTaskStatus;
begin
  Result := fStatus;
end;

function TTask.GetWaitHandle: THandle;
begin
  Result := fWaitHandle;
end;

function TTask.NeedToContinue: boolean;
begin
  Result := Assigned(fContinueWith);
end;

class function TTask.Run(proc: TaskProc): ITask;
begin
  Result := TTask.Create(proc, nil, true, nil);
  Result.Start();
end;

class function TTask.Run(proc: TaskProc; CancellationToken: ICancellationToken): ITask;
begin
  Result := TTask.Create(proc, nil, true, CancellationToken);
  Result.Start();
end;

procedure TTask.Start;
begin
  if fThread <> nil then raise Exception.Create('The task has already started');

  if NeedToContinue and fContinueOnCapturedContext and (fSynchronizationContext = nil) then begin
    fSynchronizationContext := TSynchronizationContext.Current;
  end;

  fThread := TTaskThread.Create(Self);
  fThread.FreeOnTerminate := true; //fire and forget
  fStatus := TTaskStatus.WaitingToRun;

  //bump the reference count
  //to prevent this object from being destroyed while the thread is still running
  //we will release it when the thread is done
  _AddRef;

  fThread.Start; //Resume;
end;

function ContinueCallback(const pParam : ComCallData) : HResult; stdcall;
var
  task : TTask;
begin
  Result := S_OK;
  task := nil;
  try
    task := pParam.pUserDefined;
    if task <> nil then begin
      task.DoContinue();
    end;
  except
    on E:Exception do begin
      task.fStatus := TTaskStatus.Faulted;
      task.fException := Exception(AcquireExceptionObject);
      if E is EOleSysError then Result := EOleSysError(E).ErrorCode
      else Result := E_FAIL;
    end;
  end;
end;

procedure TTask.ThreadProc;
var
  ComCallData : TComCallData;
  res: HRESULT;
  syncContext : ISynchronizationContext;
  contextCallback : IContextCallback;
begin
  fStatus := TTaskStatus.Running;

  try
    CoInitializeEx(nil, COINIT_MULTITHREADED);

    try

      //the async function
      try
        if (fCancellationToken <> nil) and fCancellationToken.IsCancellationRequested then begin
          fStatus := TTaskStatus.Canceled;
        end
        else begin
          DoExecute();
          fStatus := TTaskStatus.RanToCompletion;
        end;
      except
        on E:Exception do begin
          fStatus := TTaskStatus.Faulted;
          fException := Exception(AcquireExceptionObject);
        end;
      end;

      //continue
      if NeedToContinue then begin //don't bother if there is no continuation callback

        ZeroMemory(@ComCallData, SizeOf(ComCallData));
        ComCallData.pUserDefined := Self;

        syncContext := fSynchronizationContext;
        if syncContext <> nil then contextCallback := syncContext.ContextCallback;


        if (contextCallback = nil) (*or (syncContext.ThreadType = THDTYPE_BLOCKMESSAGES)*) then begin
          //no thread context or the thread has no message loop
          res := ContinueCallback(ComCallData);
        end
        else begin
          res := contextCallback.ContextCallback(ContinueCallback, ComCallData,
                                           IID_ICallbackWithNoReentrancyToApplicationSTA,
                                           5, nil);
        end;
        if not SUCCEEDED(res) then begin
          fException := EOleSysError.Create('IContextCallback.ContextCallback', res, 0);
          fStatus := TTaskStatus.Faulted;
        end
      end;

    finally
      fThread := nil;
      SetEvent(fWaitHandle);
      _Release;
      //final call - this object might be dead after the Release call above
    end;

  finally
   CoUninitialize();
  end;
end;

procedure TTask.Wait;
begin
  WaitForSingleObject(fWaitHandle, INFINITE);
end;

function TTask.Wait(milliseconds: integer): boolean;
begin
  Result := WaitForSingleObject(fWaitHandle, milliseconds) = WAIT_OBJECT_0;
end;

procedure WaitForTasks(const tasks: array of ITask; waitAll : boolean);
var
  handleArray : PWOHandleArray;
  i: Integer;
begin
  if Length(tasks) > MAXIMUM_WAIT_OBJECTS
    then raise Exception.CreateFmt('WhenAll: no more than %d tasks can be specified', [MAXIMUM_WAIT_OBJECTS]);

  handleArray := GetMemory(SizeOf(THandle)*Length(tasks));
  try
    for i := 0 to Length(tasks)-1 do handleArray[i] := tasks[i].WaitHandle;
    WaitForMultipleObjects(Length(tasks), handleArray, waitAll, INFINITE);
  finally
    FreeMemory(handleArray);
  end;
end;

class procedure TTask.WhenAll(const tasks: array of ITask);
begin
  WaitForTasks(tasks, true);
end;

class procedure TTask.WhenAny(const tasks: array of ITask);
begin
   WaitForTasks(tasks, false);
end;

{ TTask.TTaskThread }

constructor TTask.TTaskThread.Create(task: TTask);
begin
  inherited Create(true); //suspended, must expliciitly call Start
  fTask := task;
end;

procedure TTask.TTaskThread.Execute;
begin
  fTask.ThreadProc();
end;

{ TTask<T> }

class function TTask<T>.Async(func: TaskFunc<T>;
  continueWith: TaskProcContinue<T>;
  continueOnCapturedContext: boolean): ITask<T>;
begin
  Result := TTask<T>.Async(func, continueWith, continueOnCapturedContext, nil);
end;

class function TTask<T>.Async(func: TaskFunc<T>; continueWith: TaskProcContinue<T>): ITask<T>;
begin
  Result := TTask<T>.Async(func, continueWith, true, nil);
end;

class function TTask<T>.Async(func: TaskFunc<T>): ITask<T>;
begin
  Result := TTask<T>.Async(func, nil, true, nil);
end;

class function TTask<T>.Async(func: TaskFunc<T>;
  continueWith: TaskProcContinue<T>; continueOnCapturedContext: boolean;
  CancellationToken: ICancellationToken): ITask<T>;
begin
  Result := TTask<T>.Create(func, continueWith, continueOnCapturedContext, CancellationToken);
  Result.Start;
end;

class function TTask<T>.Async(func: TaskFunc<T>;
  continueWith: TaskProcContinue<T>;
  CancellationToken: ICancellationToken): ITask<T>;
begin
  Result := TTask<T>.Async(func, continueWith, true, CancellationToken);
end;

function TTask<T>.ConfigureAwait(continueOnCapturedContext: boolean): ITask<T>;
begin
  fContinueOnCapturedContext := continueOnCapturedContext;
  Result := Self;
end;

function TTask<T>.ContinueWith(proc: TaskProcContinue<T>; SynchronizationContext : ISynchronizationContext): ITask<T>;
begin
  fContinueWithT := proc;
  fSynchronizationContext := SynchronizationContext;
  Result := Self;
end;

class function TTask<T>.Create(func: TaskFunc<T>) : ITask<T>;
begin
  Result := CreateNew(func, nil, true, nil) as ITask<T>;
end;

function TTask<T>.ContinueWith(proc: TaskProcContinue<T>) : ITask<T>;
begin
  Result := ContinueWith(proc, nil);
end;

class function TTask<T>.Create(func: TaskFunc<T>;
  continueWith: TaskProcContinue<T>; continueOnCapturedContext: boolean;
  CancellationToken: ICancellationToken) : ITask<T>;
begin
  Result := CreateNew(func, continueWith, continueOnCapturedContext, CancellationToken) as ITask<T>;
end;

class function TTask<T>.Create(func: TaskFunc<T>; continueWith: TaskProcContinue<T>) : ITask<T>;
begin
  Result := CreateNew(func, continueWith, true, nil)  as ITask<T>;
end;

class function TTask<T>.Create(func: TaskFunc<T>;
  continueWith: TaskProcContinue<T>; continueOnCapturedContext: boolean) : ITask<T>;
begin
  Result := CreateNew(func, continueWith, continueOnCapturedContext, nil)  as ITask<T>;
end;

class function TTask<T>.Create(func: TaskFunc<T>;
  continueWith: TaskProcContinue<T>; CancellationToken: ICancellationToken) : ITask<T>;
begin
  Result := CreateNew(func, continueWith, true, CancellationToken)  as ITask<T>;
end;

constructor TTask<T>.CreateNew(func: TaskFunc<T>;
  continueWith: TaskProcContinue<T>; continueOnCapturedContext: boolean;
  CancellationToken: ICancellationToken);
begin
  inherited CreateNew(nil, nil, continueOnCapturedContext, CancellationToken);
  fFunc := func;
  fContinueWithT := continueWith;
end;

procedure TTask<T>.DoContinue;
begin
  if Assigned(fContinueWithT)
    then fContinueWithT(Self, fValue)
    else inherited DoContinue();
end;

procedure TTask<T>.DoExecute;
begin
  if Assigned(fFunc) then begin
    fValue := fFunc();
  end
  else begin
    inherited DoExecute;
  end;
end;

function TTask<T>.GetValue: T;
begin
  if fException <> nil then begin
    try
      raise fException;
    finally
      fException := nil; //since it is no longer valid
    end;
  end;

  Result := fValue;
end;

function TTask<T>.NeedToContinue: boolean;
begin
  Result := Assigned(fContinueWithT) or inherited NeedToContinue;
end;

procedure TTask<T>.SetValue(Value: T);
begin
  fValue := Value;
end;

(*
{ TTaskScheduler }

constructor TTaskScheduler.Create;
begin

end;

class function TTaskScheduler.Current: ITaskScheduler;
begin

end;

class function TTaskScheduler.Default: ITaskScheduler;
begin

end;

function TTaskScheduler.GetContextCallback: IContextCallback;
begin

end;

function TTaskScheduler.GetId: string;
begin

end;
*)

{ TSynchronizationContext }

constructor TSynchronizationContext.CreateNew;
var
  res: HRESULT;
  unk: IUnknown;
  ComThreadingInfo: IComThreadingInfo;
begin
  inherited Create;

  //Thread type
  res := CoGetObjectContext(IID_IComThreadingInfo, unk);
  if not SUCCEEDED(res) then raise EOleSysError.Create('CoGetObjectContext(IID_IComThreadingInfo)', res, 0);
  res := unk.QueryInterface(IComThreadingInfo, ComThreadingInfo);
  if not SUCCEEDED(res) then raise EOleSysError.Create('IUnknown.QueryInterface(IID_IComThreadingInfo)', res, 0);
  res := ComThreadingInfo.GetCurrentThreadType(fThreadType);
  if not SUCCEEDED(res) then raise EOleSysError.Create('IComThreadingInfo.GetCurrentThreadType', res, 0);

  //IContextCallback
  res := CoGetObjectContext(IID_IContextCallback, unk);
  if not SUCCEEDED(res) then raise EOleSysError.Create('CoGetObjectContext(IID_IContextCallback)', res, 0);
  res := unk.QueryInterface(IContextCallback, fContextCallback);
  if not SUCCEEDED(res) then raise EOleSysError.Create('IUnknown.QueryInterface(IID_IContextCallback)', res, 0);
end;

class function TSynchronizationContext.Current: ISynchronizationContext;
begin
  Result := CreateNew as ISynchronizationContext;
end;

function TSynchronizationContext.GetContextCallback: IContextCallback;
begin
  Result := fContextCallback;
end;

function TSynchronizationContext.GetThreadType: THDTYPE;
begin
  Result := fThreadType;
end;

initialization
end.
