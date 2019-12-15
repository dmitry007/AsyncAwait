program AsyncAwaitDemoCmd;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Windows, ActiveX, SysUtils, ComObj,
  AsyncAwait;

var
  t : ITask;
begin

  try
    //Since we don't use VCL in a command line app,
    //CoInitialize is not called, it is our responsibility to do so.
    CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
    try
      writeln(Format('Starting a task on thread %d', [GetCurrentThreadId]));
      t := TTask.Async(
         //code to run asynnchronously
         procedure
         begin
           writeln(Format('Running task on thread %d', [GetCurrentThreadId]));
         end,
         //code to run when the task above is completed
         procedure (task : ITask)
         begin
           writeln(Format('Finished task on thread %d', [GetCurrentThreadId]));
         end,
         //pass false to indicate that the continuations should not run on the main thread,
         //otherwise Wait / When below will lock
         false);

      //t.Wait();
      TTask.WhenAll([t]);
      writeln(Format('Finished waiting for task on thread %d', [GetCurrentThreadId]));
      t := nil;
    finally
      CoUninitialize();
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
