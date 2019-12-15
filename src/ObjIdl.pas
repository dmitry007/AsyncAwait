unit ObjIdl;

interface

uses
  Windows;
const
  IID_ICallbackWithNoReentrancyToApplicationSTA : TGUID = '{0A299774-3E4E-FC42-1D9D-72CEE105CA57}';

  //const GUID IID_IEnterActivityWithNoLock = { 0xd7174f82, 0x36b8, 0x4aa8, { 0x80, 0x0a, 0xe9, 0x63, 0xab, 0x2d, 0xfa, 0xb9 } };
  IID_IEnterActivityWithNoLock :                  TGUID = '{d7174f82-36b8-4aa8-0a80-e963ab2dfab9}';

type
  APTTYPE = DWORD;
const
  APTTYPE_CURRENT	= -1;
  APTTYPE_STA	= 0;
  APTTYPE_MTA	= 1;
  APTTYPE_NA	= 2;
  APTTYPE_MAINSTA	= 3;

type
  THDTYPE = DWORD;
const
  THDTYPE_BLOCKMESSAGES	= 0;
  THDTYPE_PROCESSMESSAGES	= 1;

const
  IID_IComThreadingInfo : TGUID = '{000001ce-0000-0000-C000-000000000046}';
  strIID_IComThreadingInfo      = '{000001ce-0000-0000-C000-000000000046}';
type
  IComThreadingInfo = interface(IUnknown)
  [strIID_IComThreadingInfo]
    function GetCurrentApartmentType(out pAptType : APTTYPE) : HResult;stdcall;
    function GetCurrentThreadType(out pThreadType : THDTYPE) : HResult;stdcall;
    function GetCurrentLogicalThreadId(out pguidLogicalThreadId : TGUID) : HResult;stdcall;
    function SetCurrentLogicalThreadId(const rguid : TGUID) : HResult;stdcall;
  end;

implementation

end.
