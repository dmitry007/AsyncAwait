unit ctxtcall;

interface

uses
  Windows;

type

  ComCallData = record
    dwDispid : DWORD;
    dwReserved : DWORD;
    pUserDefined : pointer;
  end;
  TComCallData = ComCallData;
  PComCallData = ^TComCallData;

const
  strIID_IContextCallback = '{000001da-0000-0000-C000-000000000046}';
  IID_IContextCallback : TGUID = '{000001da-0000-0000-C000-000000000046}';

type

  PFNCONTEXTCALL = function(const pParam : ComCallData) : HResult; stdcall;

  IContextCallback = interface(IUnknown)
  [strIID_IContextCallback]
    function ContextCallback(pfnCallback : PFNCONTEXTCALL;
                            const pParam : ComCallData;
                            const riid : TGUID;
                            iMethod : integer;
                            pUnk : IUnknown) : HRESULT; stdcall;

  end;

implementation

end.
