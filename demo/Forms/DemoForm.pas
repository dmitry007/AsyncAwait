unit DemoForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  AsyncAwait;

type
  TForm1 = class(TForm)
    btnDemo: TButton;
    btnFibonacci: TButton;
    lblResult: TLabel;
    procedure btnDemoClick(Sender: TObject);
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
  btnFibonacci.Enabled := false;// we will re-enable it when we are done
  TTask<UInt64>.Async(
              //timeconsuming call on a secondary thread
              function : UInt64
              begin
                Result := Fibonacci(seed);
                //raise Exception.Create('Error Message');
              end,
              //update the UI on the main thread
              procedure(task : ITask<UInt64>; Value : UInt64)
              begin
                if task.Status = TTaskStatus.RanToCompletion
                  then lblResult.Caption := Format('Fibonacci number for %d is %d', [seed, Value])
                else if task.Status = TTaskStatus.Faulted
                  then lblResult.Caption := Format('Error calculating Fibonacci number: %s', [task.Exception.Message]);
                //re-enable the button - we are done
                btnFibonacci.Enabled := true;
              end,
              //tell to continue on the main thread -
              //don't need to specify this parameter as it defaults to true anyway
              true
              );
end;

procedure TForm1.btnDemoClick(Sender: TObject);
var
  t : ITask<integer>;
  syncContext : ISynchronizationContext;
begin

  btnDemo.Enabled := false; //we will reenable it when we are done

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
                   //now start the task after we configured it;
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
                  then ShowMessage(Format('Task faulted. Exception type = %s: %s', [task.Exception.ClassName, task.Exception.Message]));
                //re-enable the buttton
                btnDemo.Enabled := true;
              end,
              //continue on the main thread - we don't need to specify this parameter
              //as it defaults to true anyway
              true
                );
  if t.Wait(20000) //Wait is smart enough not to block on continuation call above executed on the same thread
    then ShowMessage(Format('Done after waiting for less than 20 seconds. Task returned %d', [t.Value]))
    else ShowMessage('Still not done after waiting for 2 seconds');
end;

end.
