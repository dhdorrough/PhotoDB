unit Protocol_Decl;

interface

type
  TProtocol = (pUnknown, pHTTP, pHTTPs);

  TProtoColInfo = record
                    Value: string;
                  end;

var
  ProtoColInfoArray: array[TProtocol] of TProtoColInfo = (
    ({pUnknown} Value: 'unknown'),
    ({pHTTP}    Value: 'http'),
    ({phttps}   Value: 'https')
  );
  

implementation

end.
