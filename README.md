# AsyncAwait
This library (Delphi 10 or higher)  is a loose equivalent of the Task and Task&lt;> objects in .Net that uses COM infrastructure (IContextCallback.ContextCallback) to synchronize continuation calls on the main or any arbitrary thread. Works in both exe and dll projects and (unlike TThread.Synchornize) does not rely on periodic calls to CheckSynchronize.

