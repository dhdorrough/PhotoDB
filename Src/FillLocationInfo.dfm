�
 TFRMFILLLOCATIONINFO 0�  TPF0TfrmFillLocationInfofrmFillLocationInfoLeftTop� Width�Height!CaptionFill Location InfoColor	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style Menu	MainMenu1OldCreateOrderPositionpoScreenCenterOnShowFormShow
DesignSize��  PixelsPerInch`
TextHeight TButtonbtnOkLeftTop� WidthKHeightAnchorsakRightakBottom Caption&OkDefault	ModalResultTabOrderOnClick
btnOkClick  TButton	btnCancelLeftZTop� WidthKHeightAnchorsakRightakBottom Cancel	Caption&CancelModalResultTabOrder  	TCheckBoxcbUpdateExistingLocationIDLeftTop� Width� HeightCaption&Update existing location IDTabOrder  	TComboBoxlbLocationSourceLeftTop{Width� HeightStylecsDropDownList
ItemHeightTabOrder Items.Strings
0: Unknown1: Photo EXIF2: GPS Logs	3: Manual   TPanel	pnlLatLonLeft Top Width�Height9AnchorsakLeftakTopakRight 
BevelOuterbvNoneTabOrder TLabelLabel1LeftTopWidth&HeightCaptionLatitude  TLabelLabel2Left� TopWidth/HeightCaption	Longitude  TLabelLabel4LeftxTopWidthNHeightCaptionLongitude (deg,mm.mmmm)WordWrap	  TLabelLabel5LeftTopWidthFHeightCaptionLatitude (deg,mm.mmm)WordWrap	  TLabellblMaxDistanceFeetLeft TopWidthFHeight	AlignmenttaCenterAutoSizeCaptionMax Distance (Feet)WordWrap	  TOvcNumericFieldovcLatitudeLeftTop WidthaHeightCursorcrIBeamDataType	nftDoubleCaretOvr.ShapecsBlockEFColors.Disabled.BackColorclWindowEFColors.Disabled.TextColor
clGrayTextEFColors.Error.BackColorclRedEFColors.Error.TextColorclBlackEFColors.Highlight.BackColorclHighlightEFColors.Highlight.TextColorclHighlightTextOptions PictureMask###.#######TabOrder OnChangeovcLatitudeChange	RangeHigh

   s�۹����CRangeLow

   s�۹�����  TOvcNumericFieldovcLongitudeLeft� Top WidthaHeightCursorcrIBeamDataType	nftDoubleCaretOvr.ShapecsBlockEFColors.Disabled.BackColorclWindowEFColors.Disabled.TextColor
clGrayTextEFColors.Error.BackColorclRedEFColors.Error.TextColorclBlackEFColors.Highlight.BackColorclHighlightEFColors.Highlight.TextColorclHighlightTextOptions PictureMask####.#######TabOrderOnChangeovcLongitudeChange	RangeHigh

   s�۹����CRangeLow

   s�۹�����  TOvcSimpleFieldovcLatDegMinLeftTop WidthaHeightCursorcrIBeamDataType	sftStringCaretOvr.ShapecsBlockControlCharColorclRedDecimalPlaces EFColors.Disabled.BackColorclWindowEFColors.Disabled.TextColor
clGrayTextEFColors.Error.BackColorclRedEFColors.Error.TextColorclBlackEFColors.Highlight.BackColorclHighlightEFColors.Highlight.TextColorclHighlightTextOptions PictureMaskXTabOrderOnChangeovcLatDegMinChange  TOvcSimpleFieldovcLongDegMinLeftxTop WidthaHeightCursorcrIBeamDataType	sftStringCaretOvr.ShapecsBlockControlCharColorclRedDecimalPlaces EFColors.Disabled.BackColorclWindowEFColors.Disabled.TextColor
clGrayTextEFColors.Error.BackColorclRedEFColors.Error.TextColorclBlackEFColors.Highlight.BackColorclHighlightEFColors.Highlight.TextColorclHighlightTextOptions PictureMaskXTabOrderOnChangeovcLongDegMinChange  TOvcNumericFieldovcMaxDistanceFeetLeft Top WidthQHeightCursorcrIBeamDataType	nftDoubleCaretOvr.ShapecsBlockEFColors.Disabled.BackColorclWindowEFColors.Disabled.TextColor
clGrayTextEFColors.Error.BackColorclRedEFColors.Error.TextColorclBlackEFColors.Highlight.BackColorclHighlightEFColors.Highlight.TextColorclHighlightTextOptions PictureMask
##########TabOrder	RangeHigh

   s�۹����CRangeLow

   s�۹�����   TPanelpnlDescLeft Top@Width�Height9AnchorsakLeftakTopakRight 
BevelOuterbvNoneTabOrder TLabelLabel3LeftTopWidth5HeightCaption&Description  TLabelLabel6Left+TopWidthHeightCaption&State  TOvcSimpleFieldovcDescriptionLeftTopWidthHeightCursorcrIBeamDataType	sftStringCaretOvr.ShapecsBlockControlCharColorclRedDecimalPlaces EFColors.Disabled.BackColorclWindowEFColors.Disabled.TextColor
clGrayTextEFColors.Error.BackColorclRedEFColors.Error.TextColorclBlackEFColors.Highlight.BackColorclHighlightEFColors.Highlight.TextColorclHighlightText	MaxLengthFPictureMaskX	PopupMenu
PopupMenu1TabOrder OnExitovcDescriptionExit  TOvcSimpleFieldovcStateLeft+TopWidthHeightCursorcrIBeamDataType	sftStringCaretOvr.ShapecsBlockControlCharColorclRedDecimalPlaces EFColors.Disabled.BackColorclWindowEFColors.Disabled.TextColor
clGrayTextEFColors.Error.BackColorclRedEFColors.Error.TextColorclBlackEFColors.Highlight.BackColorclHighlightEFColors.Highlight.TextColorclHighlightText	MaxLengthPictureMaskATabOrder   	TCheckBoxcbPrivateLocationLeft� Top}WidthiHeightCaption&Private LocationTabOrder  TPanelpnlRefCountLeft+Top{Width� Height
BevelOuterbvNoneTabOrder TLabelLabel9Left	TopWidth3HeightCaption
Ref Count:  TLabellblRefCountLeftETopWidth7HeightCaptionlblRefCount   	TGroupBoxpnlStartDirectionLeft�TopsWidth� Height/CaptionStarting DirectionTabOrder TLabelLabel10LeftHTopWidth� HeightCaption0=N, 90=E, 180=S, 270=W  TOvcNumericFieldovcStartDirectionLeftTopWidth!HeightCursorcrIBeamDataType
nftLongIntCaretOvr.ShapecsBlockEFColors.Disabled.BackColorclWindowEFColors.Disabled.TextColor
clGrayTextEFColors.Error.BackColorclRedEFColors.Error.TextColorclBlackEFColors.Highlight.BackColorclHighlightEFColors.Highlight.TextColorclHighlightTextOptions PictureMask999TabOrder 	RangeHigh

   h        RangeLow

               TOvcSpinnerOvcSpinner1Left,TopWidthHeight
AutoRepeat	Delta       �@FocusedControlovcStartDirection   TPanelpnlDatesLeftTop� Width� Height)
BevelOuterbvNoneTabOrder	 TLabelLabel7LeftTopWidth9HeightCaption
Date Added  TLabellblDateAddedLeft`TopWidth@HeightCaptionlblDateAdded  TLabelLabel8LeftTopWidthCHeightCaptionDate Updated  TLabellblDateUpdatedLeft`TopWidthJHeightCaptionlblDateUpdated   	TMainMenu	MainMenu1Left@ 	TMenuItemEdit1Caption&Edit 	TMenuItemCopyLocationInfo1CaptionCopy Location InfoOnClickCopyLocationInfo1Click  	TMenuItemPasteLocationInfo1CaptionPaste Location InfoOnClickPasteLocationInfo1Click  	TMenuItemDefaulttoLastLocation1CaptionDefault to Last LocationShortCutD@OnClickDefaulttoLastLocation1Click    
TPopupMenu
PopupMenu1Left`Top8 	TMenuItemCut1CaptionCu&tShortCutX@OnClick	Cut1Click  	TMenuItemCopy1Caption&CopyShortCutC@OnClick
Copy1Click  	TMenuItemPaste1Caption&PasteShortCutV@OnClickPaste1Click  	TMenuItemPasteSpecial1CaptionPaste &Filtered...OnClickPasteSpecial1Click    