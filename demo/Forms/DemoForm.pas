unit DemoForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  AsyncAwait;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

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
                     ).Start;
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
              end
                );
  if t.Wait(5000)
    then ShowMessage(Format('Done after waiting for 5 seconds. Task returned %d', [t.Value]))
    else ShowMessage('Still not done after waiting for 5 seconds');
end;

end.
