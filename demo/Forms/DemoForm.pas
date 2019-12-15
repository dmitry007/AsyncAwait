unit DemoForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  AsyncAwait;

type
  TForm1 = class(TForm)
    Button1: TButton;
    btnFibonacci: TButton;
    lblResult: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure btnFibonacciClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

//don't use recursive implementation
function Fibonacci(aNumber: UInt64): Int64;
var
  I,
  N_1,
  N_2,
  N: UInt64;
begin
  case aNumber of
    0: Result:= 0;
    1: Result:= 1;
    else begin
      N_1:= 0;
      N_2:= 1;
      N := 1;
      for I:=2 to aNumber do begin
        N:= N_1 + N_2;
        N_1:= N_2;
        N_2:= N;
      end;
      Result:= N;
    end;
  end;
end;

procedure TForm1.btnFibonacciClick(Sender: TObject);
var
  seed: UInt64;
begin
  seed := Random(MaxInt);
  lblResult.Caption := Format('Calculating Fibonacci number for %d. Please wait...', [seed]);
  TTask<UInt64>.Async(
              //timeconsuming call on a secondary thread
              function : UInt64
              begin
                Result := Fibonacci(seed);
              end,
              //update the UI on the main thread
              procedure(task : ITask<UInt64>; Value : UInt64)
              begin
                if task.Status = TTaskStatus.RanToCompletion
                  then lblResult.Caption := Format('Fibonacci number for %d is %d', [seed, Value])
                else if task.Status = TTaskStatus.Faulted
                  then lblResult.Caption := Format('Error calculating Fibonacci number: %s', [task.Exception.Message])
              end,
              //tell to continue on the main thread -
              //don't need to specify this parameter as it defaults to true anyway
              true
              );
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  t : ITask<integer>;
  syncContext : ISynchronizationContext;
begin
  syncContext := TSynchronizationContext.Current;

  t := TTask<integer>.Async(

              //async function
              function() : integer
              var
                delay : integer;
              begin
                delay := Random(10000);
                sleep(delay);
                Result := delay;
                //run another task
                //start it explicitly since we need to configure the
                //synchornization context (main thread) first
                //before the continuation code has a chance to run
                TTask.Create(
                     procedure()
                     begin
                       sleep(2000);
                     end
                   ).
                   ContinueWith(
                     procedure(task : ITask)
                     begin
                       ShowMessage('Done second task');
                     end,
                     //use the sync context from the main thread since we are showing a window
                     syncContext
                     ).
                   //now start the task after we configured it
                   Start;
              end,

              //continue with
              //it is guaranteed to run on the calling thread (this main thread)
              //since continueOnCapturedContext parameter defaults to true
              procedure(task : ITask<integer>; Value : integer )
              begin
                if task.Status = TTaskStatus.RanToCompletion
                  then ShowMessage(Format('All done after sleeping for %d seconds', [task.Value div 1000]))
                else if task.Status = TTaskStatus.Faulted
                  then ShowMessage(Format('Task faulted. Exception type = %s: %s', [task.Exception.ClassName, task.Exception.Message]))
              end,
              //continue on the main thread - don't need to specify this parameter as it defaults to true
              true
                );
  if t.Wait(2000)
    then ShowMessage(Format('Done after waiting for less than 2 seconds. Task returned %d', [t.Value]))
    else ShowMessage('Still not done after waiting for 2 seconds');
end;

end.
