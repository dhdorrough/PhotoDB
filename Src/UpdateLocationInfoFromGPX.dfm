�
 TFRMUPDATELOCATIONINFO 0T  TPF0TfrmUpdateLocationInfofrmUpdateLocationInfoLeft�TopIWidthHeight3AnchorsakLeftakBottom Caption&Update Location info from .gpx file(s)Color	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style OldCreateOrderPositionpoScreenCenter
DesignSize� PixelsPerInch`
TextHeight TLabel	lblStatusLeftTop�Width(HeightCaption	lblStatus  TLabelLabel1LeftTopWidthnHeightCaptionLowest Date to Include  TLabelLabel2Left� TopnWidth� HeightCaption#Minutes to add to Photo Date / Time  TLabellblCountLeftTop/Width&HeightCaptionlblCount  TLabellblLowestDateLeftTopGWidthCHeightCaptionlblLowestDate  TLabelLabel3LeftTop4WidthpHeightCaptionHighest Date to Include  TButtonbtnUpdateSelectedLeftcTop�Width� HeightAnchorsakRightakBottom CaptionUpdate Selected RecordsEnabledTabOrderOnClickbtnUpdateSelectedClick  TPanelpnlGPXLeftToprWidth�HeightaAnchorsakLeftakTopakRight 
BevelInnerbvRaised
BevelOuter	bvLoweredBorderStylebsSingleTabOrder
DesignSize�]  TLabelLabel4LeftTopHWidthcHeightCaptione.g. 2014,2015,2016  TLabeledEditleGpxFilterLeft
TopWidth!HeightAnchorsakLeftakTopakRight EditLabel.Width*EditLabel.HeightEditLabel.Caption.gpx filterTabOrder Text=F:\NDAS-I\Documents\Delorme\DeLorme Docs\Transfer Files\*.gpxOnChangeleGpxFilterChange  TBitBtn	btnLocateLeft2TopWidthKHeightAnchorsakRightakBottom CaptionLocateTabOrderOnClickbtnLocateClick  TButtonbtnLoadFromGPXLeft~TopWidthKHeightAnchorsakRightakBottom CaptionLoadTabOrderOnClickbtnLoadFromGPXClick  	TCheckBoxcbScanSelectedSubFoldersLeftTop2Width� HeightCaptionScan Selected SubFoldersTabOrderOnClickcbScanSelectedSubFoldersClick  TLabeledEditleSelectedFoldersLeftTop0Width2HeightEditLabel.Width_EditLabel.HeightEditLabel.CaptionSelected SubfoldersEnabledLabelPositionlpLeftLabelSpacing
TabOrder   TPanelpnlCSVLeftTop� Width�Height7AnchorsakLeftakTopakRight 
BevelInnerbvRaised
BevelOuter	bvLoweredBorderStylebsSingleTabOrder
DesignSize�3  TLabeledEditleTrackDataLfnLeft
TopWidth!HeightAnchorsakLeftakTopakRight EditLabel.WidthIEditLabel.HeightEditLabel.CaptionTrack Data FileTabOrder   TBitBtnbtnLocateTrackDataLeft2TopWidthKHeightAnchorsakRightakBottom CaptionLocateTabOrderOnClickbtnLocateTrackDataClick  TButtonbtnLoadTrackDataLeft~TopWidthKHeightAnchorsakRightakBottom CaptionLoadTabOrderOnClickbtnLoadTrackDataClick   TRadioButtonrbLoadFromGPXLeftTop`WidthqHeightCaptionLoad From GPXTabOrderOnClickrbLoadFromGPXClick  TRadioButtonrbLoadFromCSVLeftTop� Width� HeightCaptionLoad From .csv Track DataTabOrderOnClickrbLoadFromGPXClick  TOvcPictureFieldovcLowestDateLeftTopWidthYHeightCursorcrIBeamDataTypepftDateCaretOvr.ShapecsBlockControlCharColorclRedDecimalPlaces EFColors.Disabled.BackColorclWindowEFColors.Disabled.TextColor
clGrayTextEFColors.Error.BackColorclRedEFColors.Error.TextColorclBlackEFColors.Highlight.BackColorclHighlightEFColors.Highlight.TextColorclHighlightTextEpoch InitDateTime	MaxLength
OptionsefoCaretToEnd PictureMask
MM/DD/yyyyTabOrder OnChangeovcLowestDateChange	RangeHigh

   �f       RangeLow

   <         	TCheckBoxcbAdjustPhotoTimeLeftTopjWidthyHeightCaptionAdjust Photo TimeTabOrder  TOvcNumericFieldovcMinutesToAddLeft� TopjWidthQHeightCursorcrIBeamDataType	nftDoubleCaretOvr.ShapecsBlockEFColors.Disabled.BackColorclWindowEFColors.Disabled.TextColor
clGrayTextEFColors.Error.BackColorclRedEFColors.Error.TextColorclBlackEFColors.Highlight.BackColorclHighlightEFColors.Highlight.TextColorclHighlightTextOptions PictureMask
##########TabOrder		RangeHigh

          �	@RangeLow

          �	�  	TCheckBox
cbSimulateLeft	Top|WidthaHeightCaptionSimulate OnlyTabOrder
OnClickcbSimulateClick  	TGroupBoxrgUseDateFromLeft� TopWidth� Height;CaptionUse Date/Time fromTabOrder 	TCheckBox
cbEXIFDataLeftTopWidthqHeightCaptionPhoto EXIF DataChecked	State	cbCheckedTabOrder OnClickcbEXIFDataClick  	TCheckBox
cbDataBaseLeftTop#WidthaHeightCaption	Data BaseChecked	State	cbCheckedTabOrderOnClickcbDataBaseClick   TOvcPictureFieldovcHighestDateLeftTopDWidthYHeightCursorcrIBeamDataTypepftDateCaretOvr.ShapecsBlockControlCharColorclRedDecimalPlaces EFColors.Disabled.BackColorclWindowEFColors.Disabled.TextColor
clGrayTextEFColors.Error.BackColorclRedEFColors.Error.TextColorclBlackEFColors.Highlight.BackColorclHighlightEFColors.Highlight.TextColorclHighlightTextEpoch InitDateTime	MaxLength
OptionsefoCaretToEnd PictureMask
MM/DD/yyyyTabOrderOnChangeovcHighestDateChange	RangeHigh

   �f       RangeLow

   <         TButtonbtnClearPreviouslyLoadedTracksLeft8TopPWidth� HeightCaptionClear Previously Loaded TracksTabOrderOnClick#btnClearPreviouslyLoadedTracksClick  TButtonbtnSaveTrackDataLeft�Top�Width� HeightAnchorsakRightakBottom CaptionSave Track Data FileTabOrderOnClickbtnSaveTrackDataClick  TButtonbtnSaveTrackDataAsLeft0Top�Width� HeightAnchorsakRightakBottom CaptionSave Track Data File As...TabOrderOnClickbtnSaveTrackDataAsClick  TLabeledEditleDefaultLocationDescriptionLeftTop�WidthaHeightEditLabel.Width� EditLabel.HeightEditLabel.CaptionDefault Location DescriptionTabOrder  TLabeledEditleDefaultLocationStateLeft�Top�Width,HeightEditLabel.WidthjEditLabel.HeightEditLabel.CaptionDefault Location StateTabOrder  	TCheckBoxcbConfirmEachUpdateLeftTop�Width� HeightCaptionConfirm Each UpdateTabOrder  TOpenDialogOpenDialog1
DefaultExtcsvFilterComma Delimited (*.csv)|*.csvLeft�Top�    