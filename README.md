# AsyncAwait
This library (Delphi 10 or higher) exposes loose equivalents of the **Task** and **Task&lt;gt;** objects in .Net that uses COM infrastructure (**IContextCallback.ContextCallback**) to synchronize continuation calls on the main or any arbitrary thread. Works in both exe and dll projects and (unlike **TThread.Synchornize**) does not rely on periodic calls to **CheckSynchronize**.

The source code is licensed under the MIT license.

To use this library, add **AsyncAwait** to the uses clause; you can then create code like the following:

```pascal
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
```
You can also specify a particular synchronization context even if you start a new task on a secondary thread:
```pascal
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
```
