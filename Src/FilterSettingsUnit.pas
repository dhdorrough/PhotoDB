unit FilterSettingsUnit;

interface

uses
  SettingsFiles, PDB_Decl, PDBTables;

type
  TFilterSettings = class(TSettingsFile)
  private
    fHasLocationInfoInEXIF: boolean;
    fAllowStrSimilarity: boolean;
    fHasSound: boolean;
    fHasLocationID: boolean;
    fUnprocessedOnly: boolean;
    fMatchWholeWordsOnly: boolean;
    fNotCopyCode: boolean;
    fHighYear: string;
    fLowMonth: string;
    fHighMonth: string;
    fLowYear: string;
    fDateType: TDateTypes;
    fCopyCode: string;
    fKeyWords: string;
    fStringInPath: string;
    fStringInFileName: string;
    fLowDate: string;
    fHighDate: string;
    fFilterExpression: string;
    fMediaClasses: TMediaClasses;
    fFileIsMissing: boolean;
    fScanComments: boolean;
    fScanFile: boolean;
    fMinStrSimilarity: integer;
    fUseSynonyms: boolean;
    fCurrentSortOrder: TCurrentOrder;
    fHasSceneInfo: boolean;
    fScanSceneDates: boolean;
    fScanSceneKeyWords: boolean;
    fFilePathNo: integer;
    fIncludeSubFolders: boolean;
    fSelectedFolder: string;
    procedure SetMatchWholeWordsOnly(const Value: boolean);
  published
    property StringInPath: string
             read fStringInPath
             write fStringInPath;
    property StringInFileName: string
             read fStringInFileName
             write fStringInFileName;
    property LowDate: string
             read fLowDate
             write fLowDate;
    property HighDate: string
             read fHighDate
             write fHighDate;
    property DateType: TDateTypes
             read fDateType
             write fDateType;
    property LowYear: string
             read fLowYear
             write fLowYear;
    property HighYear: string
             read fHighYear
             write fHighYear;
    property LowMonth: string
             read fLowMonth
             write fLowMonth;
    property HighMonth: string
             read fHighMonth
             write fHighMonth;
    property CopyCode: string
             read fCopyCode
             write fCopyCode;
    property FileIsMissing: boolean
             read fFileIsMissing
             write fFileIsMissing;
    property NotCopyCode: boolean
             read fNotCopyCode
             write fNotCopyCode;
    property KeyWords: string
             read fKeyWords
             write fKeyWords;
    property MatchWholeWordsOnly: boolean
             read fMatchWholeWordsOnly
             write SetMatchWholeWordsOnly;
    property ScanComments: boolean
             read fScanComments
             write fScanComments;
    property ScanFile: boolean
             read fScanFile
             write fScanFile;
    property AllowStrSimilarity: boolean
             read fAllowStrSimilarity
             write fAllowStrSimilarity;
    property MinStrSimilarity: integer
             read fMinStrSimilarity
             write fMinStrSimilarity;
    property UseSynonyms: boolean
             read fUseSynonyms
             write fUseSynonyms;
    property UnprocessedOnly: boolean
             read fUnprocessedOnly
             write fUnprocessedOnly;
    property HasSound: boolean
             read fHasSound
             write fHasSound;
    property HasLocationID: boolean
             read fHasLocationID
             write fHasLocationID;
    property HasLocationInfoInEXIF: boolean
             read fHasLocationInfoInEXIF
             write fHasLocationInfoInEXIF;
    property HasSceneInfo: boolean
             read fHasSceneInfo
             write fHasSceneInfo;
    property FilterExpression: string
             read fFilterExpression
             write fFilterExpression;
    property MediaClasses: TMediaClasses
             read fMediaClasses
             write fMediaClasses;
    property CurrentSortOrder: TCurrentOrder
             read fCurrentSortOrder
             write fCurrentSortOrder;
    property ScanSceneKeyWords: boolean
             read fScanSceneKeyWords
             write fScanSceneKeyWords;
    property ScanSceneDates: boolean
             read fScanSceneDates
             write fScanSceneDates;
    property FilePathNo: integer
             read fFilePathNo
             write fFilePathNo;
    property IncludeSubFolders: boolean
             read fIncludeSubFolders
             write fIncludeSubFolders;
    property SelectedFolder: string
             read fSelectedFolder
             write fSelectedFolder;
  end;

implementation

{ TFilterSettings }

procedure TFilterSettings.SetMatchWholeWordsOnly(const Value: boolean);
begin
  fMatchWholeWordsOnly := Value;
end;

end.
