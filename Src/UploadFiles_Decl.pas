unit UploadFiles_Decl;

interface

const
{$IfDef DHD}
  FTP_URL          = 'ruthanddan.net';
  FTP_USERNAME     = 'u37434010';
  FTP_PASSWORD     = 'fairbanks';
  REMOTE_FILE_PATH = '/u37434010/PhotoDB';
{$else}
  FTP_URL          = '';
  FTP_USERNAME     = '';
  FTP_PASSWORD     = '';
  REMOTE_FILE_PATH = '';
{$EndIf}

implementation

end.
