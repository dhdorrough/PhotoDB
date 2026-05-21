unit CSS_Decl;

interface

(*
const
{$IfDef DHD}
  CLOCALCSSLFN   = 'F:/NDAS-I/MyWeb3/MyStyle.css';
  CREMOTECSSLFN  = 'http://RuthAndDan.net/MyStyle.css';
  CLOCALHOME     = 'P:/Default.htm';
{$else}
  CLOCALCSSLFN   = '';
  CREMOTECSSLFN  = '';
  CLOCALHOME     = '';
{$EndIf}
*)

type
  TCSSLocale = (cl_Unknown, cl_Local, cl_Remote);


implementation

end.
