object frmPhotoDataBase: TfrmPhotoDataBase
  Left = 410
  Top = 146
  Width = 1325
  Height = 673
  Caption = 'Photo Database'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    00000000000000000000000000000000000000000FFFFFFFFFF0000000000000
    00000000FFFFFFFFFFFF00000000000000000000FFFFFFFFFFFFFFF000000000
    000000000FFFFFFFFFFFFFFFF00000000000000000FFFFF00FFFFFFFFF000000
    00000000000FFFF000FFFFFFFF00000000000000000FFFF000000FFFFF000000
    00000000000FFFF000000FFFFF00000000000000000FFFF00000FFFFF0000000
    FF000000000FFFF0000FFFFFF000000FFFF00000000FFFF0FFFFFFFF0000000F
    FFF00000000FFFFFFFFFFFF00000000FFFF00000FFFFFFFFFFFFFF000000000F
    FFF0000FFFFFFFFFFFFF00000000000FFFFF000FFFFFFFFFFF0000000000000F
    FFFF0000FFFFFFFF0000000000000000FFFFFFF0000000000000000000000000
    FFFFFFFFFF0000000000000000000000FFFFFFFFFFFFF0000000000000000000
    FFFFFFFFFFFFFF000000000000000000FFFF0FFFFFFFFFF00000000000000000
    FFFF00000FFFFFF00000000000000000FFFF0000000FFFFF000000000000000F
    FFFF0000000FFFFF000000000000000FFFFF000000FFFFFF00000000000000FF
    FFFFFFFFFFFFFFF00000000000000FFFFFFFFFFFFFFFFF000000000000000FFF
    FFFFFFFFFFFFF0000000000000000FFFFFFFFFFFFFFF000000000000000000FF
    FFF000000000000000000000000000000000000000000000000000000000FFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
  Menu = MainMenu1
  OldCreateOrder = False
  PopupMenu = PopupMenu2
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  DesignSize = (
    1309
    614)
  PixelsPerInch = 96
  TextHeight = 14
  object lblStatus: TLabel
    Left = 8
    Top = 583
    Width = 41
    Height = 14
    Anchors = [akLeft, akBottom]
    Caption = 'lblStatus'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label28: TLabel
    Left = 295
    Top = 161
    Width = 34
    Height = 11
    Caption = 'File Size'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Small Fonts'
    Font.Style = []
    ParentFont = False
  end
  object Label29: TLabel
    Left = 295
    Top = 183
    Width = 37
    Height = 14
    Caption = 'FileSize'
  end
  object lblStatus1: TLabel
    Left = 8
    Top = 597
    Width = 47
    Height = 14
    Anchors = [akLeft, akBottom]
    Caption = 'lblStatus1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object RotateImage1: TRotateImage
    Left = 1072
    Top = 16
    Width = 105
    Height = 105
  end
  object PageControl1: TPageControl
    Left = 9
    Top = 1
    Width = 1289
    Height = 580
    ActivePage = tabPhoto
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    OnChange = PageControl1Change
    object tabBrowse: TTabSheet
      Caption = '&Filter'
      DesignSize = (
        1281
        551)
      object Panel2: TPanel
        Left = 6
        Top = 585
        Width = 1291
        Height = 131
        Anchors = [akLeft, akRight, akBottom]
        BevelOuter = bvNone
        TabOrder = 0
      end
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 353
        Height = 551
        Align = alLeft
        BevelOuter = bvNone
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        DesignSize = (
          353
          551)
        object Label21: TLabel
          Left = 2
          Top = 1
          Width = 73
          Height = 15
          Caption = 'String in Path'
        end
        object Label6: TLabel
          Left = 3
          Top = 40
          Width = 105
          Height = 15
          Caption = 'String in File Name'
          FocusControl = edtFileNames
        end
        object Label7: TLabel
          Left = 1
          Top = 81
          Width = 52
          Height = 15
          Caption = '&Low Date'
          FocusControl = edtLowDate
        end
        object Label8: TLabel
          Left = 71
          Top = 81
          Width = 55
          Height = 15
          Caption = '&High Date'
          FocusControl = edtHighDate
        end
        object Label22: TLabel
          Left = 141
          Top = 81
          Width = 54
          Height = 15
          Caption = 'Date Type'
        end
        object lblLowYear: TLabel
          Left = 3
          Top = 132
          Width = 38
          Height = 12
          Caption = 'Low Year'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object lblHighYear: TLabel
          Left = 54
          Top = 132
          Width = 41
          Height = 12
          Caption = 'High Year'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object Label12: TLabel
          Left = 109
          Top = 121
          Width = 25
          Height = 24
          Caption = 'Low Month'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          WordWrap = True
        end
        object edHighMonth: TLabel
          Left = 144
          Top = 122
          Width = 25
          Height = 24
          Caption = 'High Month'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          WordWrap = True
        end
        object Label9: TLabel
          Left = 3
          Top = 173
          Width = 59
          Height = 15
          Caption = '&Key Words'
          FocusControl = edtKeyWords
        end
        object Label15: TLabel
          Left = 285
          Top = 131
          Width = 48
          Height = 12
          Anchors = [akTop, akRight]
          Caption = 'Copy Code'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object Label17: TLabel
          Left = 260
          Top = 131
          Width = 15
          Height = 12
          Anchors = [akTop, akRight]
          Caption = 'Not'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object Label33: TLabel
          Left = 290
          Top = 1
          Width = 58
          Height = 15
          Caption = 'File Path #'
        end
        object Label34: TLabel
          Left = 181
          Top = 121
          Width = 19
          Height = 24
          Caption = 'Low Day'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          WordWrap = True
        end
        object Label35: TLabel
          Left = 216
          Top = 122
          Width = 22
          Height = 24
          Caption = 'High Day'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          WordWrap = True
        end
        object edtStringInPath: TEdit
          Left = 2
          Top = 17
          Width = 282
          Height = 23
          TabOrder = 0
          OnChange = ParameterChanged
        end
        object edtFileNames: TEdit
          Left = 3
          Top = 56
          Width = 349
          Height = 23
          TabOrder = 2
          OnChange = ParameterChanged
        end
        object edtLowDate: TEdit
          Left = 3
          Top = 98
          Width = 66
          Height = 23
          TabOrder = 3
          OnChange = ParameterChanged
        end
        object edtHighDate: TEdit
          Left = 71
          Top = 98
          Width = 66
          Height = 23
          TabOrder = 4
          OnChange = ParameterChanged
        end
        object cbDateType: TComboBox
          Left = 139
          Top = 98
          Width = 128
          Height = 23
          Style = csDropDownList
          ItemHeight = 15
          TabOrder = 5
          OnChange = ParameterChanged
          Items.Strings = (
            'Photo Date'
            'Date Added'
            'Date Updated'
            'Photo Date + Time')
        end
        object edtLowYear: TEdit
          Left = 3
          Top = 146
          Width = 42
          Height = 23
          TabOrder = 6
          OnChange = ParameterChanged
        end
        object edtHighYear: TEdit
          Left = 54
          Top = 146
          Width = 42
          Height = 23
          TabOrder = 7
          OnChange = ParameterChanged
        end
        object edtLowMonth: TEdit
          Left = 109
          Top = 146
          Width = 27
          Height = 23
          TabOrder = 8
          OnChange = ParameterChanged
        end
        object edtHighMonth: TEdit
          Left = 144
          Top = 146
          Width = 26
          Height = 23
          TabOrder = 9
          OnChange = ParameterChanged
        end
        object edtKeyWords: TEdit
          Left = 2
          Top = 189
          Width = 350
          Height = 23
          TabOrder = 27
          OnChange = ParameterChanged
        end
        object cbMatchWholeWordsOnly: TCheckBox
          Left = 2
          Top = 214
          Width = 159
          Height = 17
          Caption = 'Match &Whole Words Only'
          Checked = True
          State = cbChecked
          TabOrder = 14
          OnClick = ParameterChanged
        end
        object cbHasSound: TCheckBox
          Left = 2
          Top = 446
          Width = 83
          Height = 17
          Caption = 'Has Sound'
          TabOrder = 26
          OnClick = ParameterChanged
        end
        object edtCopyCode: TMaskEdit
          Left = 286
          Top = 146
          Width = 41
          Height = 23
          Hint = 'Copyright Code'
          EditMask = '>aaaa;0; '
          MaxLength = 4
          ParentShowHint = False
          ShowHint = True
          TabOrder = 13
          OnChange = ParameterChanged
        end
        object cbNot: TCheckBox
          Left = 267
          Top = 149
          Width = 15
          Height = 17
          TabOrder = 12
          OnClick = ParameterChanged
        end
        object cbUnprocessedOnly: TCheckBox
          Left = 2
          Top = 426
          Width = 122
          Height = 17
          Caption = '&Unprocessed Only'
          TabOrder = 25
          OnClick = ParameterChanged
        end
        object btnApply: TButton
          Left = 293
          Top = 512
          Width = 61
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = '&Apply'
          Default = True
          TabOrder = 33
          OnClick = btnApplyClick
        end
        object btnClear: TButton
          Left = 226
          Top = 512
          Width = 61
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Clear'
          TabOrder = 32
          OnClick = btnClearClick
        end
        object cbUseSynonyms: TCheckBox
          Left = 2
          Top = 231
          Width = 105
          Height = 17
          Caption = 'Use Synonyms'
          TabOrder = 15
          OnClick = cbUseSynonymsClick
        end
        object cbLocationID: TCheckBox
          Left = 2
          Top = 285
          Width = 126
          Height = 17
          Caption = 'Has Location ID'
          TabOrder = 18
          OnClick = ParameterChanged
        end
        object cbHasLocationInfoInEXIF: TCheckBox
          Left = 2
          Top = 303
          Width = 160
          Height = 17
          Caption = 'Has Location Info in EXIF'
          TabOrder = 19
          OnClick = ParameterChanged
        end
        object clbMediaClasses: TCheckListBox
          Left = 178
          Top = 214
          Width = 171
          Height = 288
          OnClickCheck = clbMediaClassesClickCheck
          Anchors = [akLeft, akTop, akBottom]
          BevelEdges = []
          BevelInner = bvNone
          BevelOuter = bvNone
          ItemHeight = 15
          ParentColor = True
          TabOrder = 28
        end
        object btnExpression: TButton
          Left = 149
          Top = 512
          Width = 72
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Expression'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 31
          OnClick = btnExpressionClick
        end
        object btnLoadFilter: TButton
          Left = 5
          Top = 513
          Width = 66
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Load Filter'
          TabOrder = 29
          OnClick = btnLoadFilterClick
        end
        object btnSaveFilter: TButton
          Left = 77
          Top = 512
          Width = 66
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Save Filter'
          TabOrder = 30
          OnClick = btnSaveFilterClick
        end
        object edtFilePathNo: TEdit
          Left = 290
          Top = 17
          Width = 63
          Height = 23
          TabOrder = 1
          OnChange = edtFilePathNoChange
        end
        object cbFileIsMissing: TCheckBox
          Left = 2
          Top = 339
          Width = 151
          Height = 17
          Caption = 'File is Missing'
          TabOrder = 21
          OnClick = ParameterChanged
        end
        object cbScanComments: TCheckBox
          Left = 2
          Top = 267
          Width = 174
          Height = 17
          Caption = 'Scan Comment for KeyWords'
          TabOrder = 17
          OnClick = cbScanCommentsClick
        end
        object cbScanFile: TCheckBox
          Left = 2
          Top = 374
          Width = 174
          Height = 17
          Caption = 'Scan File for KeyWords'
          TabOrder = 22
        end
        object cbAllowStrSimilarity: TCheckBox
          Left = 2
          Top = 249
          Width = 118
          Height = 17
          Caption = 'StrCompare Match'
          TabOrder = 16
          OnClick = cbAllowStrSimilarityClick
        end
        object meSS: TOvcNumericField
          Left = 126
          Top = 244
          Width = 25
          Height = 23
          Cursor = crIBeam
          DataType = nftByte
          CaretOvr.Shape = csBlock
          EFColors.Disabled.BackColor = clWindow
          EFColors.Disabled.TextColor = clGrayText
          EFColors.Error.BackColor = clRed
          EFColors.Error.TextColor = clBlack
          EFColors.Highlight.BackColor = clHighlight
          EFColors.Highlight.TextColor = clHighlightText
          Enabled = False
          Options = [efoArrowIncDec]
          PictureMask = '99'
          TabOrder = 34
          RangeHigh = {63000000000000000000}
          RangeLow = {0A000000000000000000}
        end
        object cbIncludeSubFolders: TCheckBox
          Left = 231
          Top = 37
          Width = 121
          Height = 17
          Caption = 'Include Sub-Folders'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          TabOrder = 35
          Visible = False
        end
        object cbScanSceneKeyWords: TCheckBox
          Left = 2
          Top = 391
          Width = 174
          Height = 17
          Caption = 'Scan Scenes for KeyWords'
          TabOrder = 23
        end
        object cbHasSceneInfo: TCheckBox
          Left = 2
          Top = 321
          Width = 105
          Height = 17
          Caption = 'Has Scene Info'
          TabOrder = 20
          OnClick = cbHasSceneInfoClick
        end
        object cbScanSceneDates: TCheckBox
          Left = 2
          Top = 408
          Width = 174
          Height = 17
          Caption = 'Scan Scenes for Dates'
          TabOrder = 24
        end
        object edtLowDay: TEdit
          Left = 181
          Top = 146
          Width = 27
          Height = 23
          TabOrder = 10
          OnChange = ParameterChanged
        end
        object edtHighDay: TEdit
          Left = 216
          Top = 146
          Width = 26
          Height = 23
          TabOrder = 11
          OnChange = ParameterChanged
        end
      end
      object Panel4: TPanel
        Left = 353
        Top = 0
        Width = 928
        Height = 551
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 2
        DesignSize = (
          928
          551)
        object dbGrid1: TDBGrid
          Left = 8
          Top = 21
          Width = 905
          Height = 509
          Anchors = [akLeft, akTop, akRight, akBottom]
          DataSource = dsPhotoTable
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Arial'
          Font.Style = []
          Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit]
          ParentFont = False
          TabOrder = 0
          TitleFont.Charset = DEFAULT_CHARSET
          TitleFont.Color = clWindowText
          TitleFont.Height = -11
          TitleFont.Name = 'Arial'
          TitleFont.Style = []
          OnDblClick = dbGrid1DblClick
          OnMouseDown = dbGrid1MouseDown
          Columns = <
            item
              Expanded = False
              FieldName = 'Key'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'FILE_NAME'
              Title.Caption = 'File Name'
              Width = 153
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'PHOTODATE'
              Title.Caption = 'yyyymmdd'
              Width = 70
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'PhotoDateTime'
              Title.Caption = 'Date Time'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'ADDED'
              Title.Caption = 'Added'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'UPDATED'
              Title.Caption = 'Updated'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'LocationID'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clMenuHighlight
              Font.Height = -11
              Font.Name = 'Arial'
              Font.Style = []
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'FILE_SIZE'
              Title.Caption = 'File Size'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'KEY_WORDS'
              Title.Caption = 'Key Words'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'Width'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'Height'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'Media_Length'
              Title.Caption = 'Media Length'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'HasSound'
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'Distance'
              Title.Caption = 'Distance (MIles)'
              Width = 93
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'Latitude'
              Width = 110
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'Longitude'
              Width = 107
              Visible = True
            end
            item
              Expanded = False
              FieldName = 'TextInFile'
              Title.Caption = 'Text in File'
              Visible = True
            end>
        end
      end
    end
    object tabPhoto: TTabSheet
      Caption = '&Document'
      ImageIndex = 1
      OnResize = tabPhotoResize
      DesignSize = (
        1281
        551)
      object pnlPhoto: TPanel
        Left = 451
        Top = 25
        Width = 831
        Height = 491
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -21
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        object imgPhoto: TRotateImage
          Left = 1
          Top = 1
          Width = 829
          Height = 489
          Align = alClient
          Center = True
          PopupMenu = puImage
          Proportional = True
          OnMouseDown = imgPhotoMouseDown
          OnMouseMove = imgPhotoMouseMove
          OnStartDrag = imgPhotoStartDrag
        end
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 437
        Height = 551
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 1
        DesignSize = (
          437
          551)
        object Label4: TLabel
          Left = 4
          Top = 4
          Width = 46
          Height = 14
          Caption = 'Root Path'
        end
        object lblFilePath: TLabel
          Left = 4
          Top = 29
          Width = 40
          Height = 14
          Caption = 'File Path'
          FocusControl = dbFilePath
        end
        object Label1: TLabel
          Left = 4
          Top = 54
          Width = 46
          Height = 14
          Caption = 'File Name'
          FocusControl = dbFileName
        end
        object Label10: TLabel
          Left = 7
          Top = 74
          Width = 23
          Height = 14
          Caption = '&Year'
          FocusControl = dbYear
        end
        object Label11: TLabel
          Left = 90
          Top = 75
          Width = 29
          Height = 14
          Caption = '&Month'
          FocusControl = dbMonth
        end
        object Label18: TLabel
          Left = 215
          Top = 75
          Width = 62
          Height = 14
          Caption = 'YYYYMMDD'
          FocusControl = dbPhotoDate
        end
        object Label13: TLabel
          Left = 6
          Top = 163
          Width = 40
          Height = 14
          Caption = 'Updated'
        end
        object Label23: TLabel
          Left = 111
          Top = 163
          Width = 32
          Height = 14
          Caption = 'Added'
        end
        object Label16: TLabel
          Left = 216
          Top = 156
          Width = 24
          Height = 22
          Caption = 'Copy Code'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Small Fonts'
          Font.Style = []
          ParentFont = False
          WordWrap = True
        end
        object Label20: TLabel
          Left = 259
          Top = 161
          Width = 30
          Height = 11
          Caption = 'Private'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Small Fonts'
          Font.Style = []
          ParentFont = False
        end
        object Label2: TLabel
          Left = 6
          Top = 276
          Width = 54
          Height = 14
          Caption = '&Key Words'
          FocusControl = dbKeyWords
        end
        object Label19: TLabel
          Left = 8
          Top = 332
          Width = 44
          Height = 14
          Caption = '&Comment'
          FocusControl = dbComments
        end
        object Label5: TLabel
          Left = 144
          Top = 75
          Width = 19
          Height = 14
          Caption = '&Day'
          FocusControl = dbDay
        end
        object Label24: TLabel
          Left = 7
          Top = 121
          Width = 108
          Height = 14
          Caption = 'Document Date && Time'
        end
        object dbKey: TDBText
          Left = 42
          Top = 529
          Width = 65
          Height = 17
          Anchors = [akLeft, akBottom]
          DataField = 'Key'
          DataSource = dsPhotoTable
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object Label25: TLabel
          Left = 8
          Top = 529
          Width = 28
          Height = 14
          Anchors = [akLeft, akBottom]
          Caption = 'Key ='
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object Label14: TLabel
          Left = 7
          Top = 209
          Width = 34
          Height = 11
          Caption = 'File Size'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Small Fonts'
          Font.Style = []
          ParentFont = False
        end
        object lblFileSize: TLabel
          Left = 7
          Top = 222
          Width = 37
          Height = 14
          Caption = 'FileSize'
        end
        object Label26: TLabel
          Left = 73
          Top = 209
          Width = 23
          Height = 11
          Caption = 'Width'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Small Fonts'
          Font.Style = []
          ParentFont = False
        end
        object DBText1: TDBText
          Left = 73
          Top = 223
          Width = 31
          Height = 17
          DataField = 'Width'
          DataSource = dsPhotoTable
        end
        object Label30: TLabel
          Left = 111
          Top = 209
          Width = 27
          Height = 11
          Caption = 'Height'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Small Fonts'
          Font.Style = []
          ParentFont = False
        end
        object DBText2: TDBText
          Left = 111
          Top = 223
          Width = 31
          Height = 17
          DataField = 'Height'
          DataSource = dsPhotoTable
        end
        object Label27: TLabel
          Left = 112
          Top = 248
          Width = 37
          Height = 11
          Caption = 'Location'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Small Fonts'
          Font.Style = []
          ParentFont = False
          PopupMenu = puLocationInfo
        end
        object Label31: TLabel
          Left = 296
          Top = 161
          Width = 75
          Height = 11
          Caption = 'Scanned or Edited'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Small Fonts'
          Font.Style = []
          ParentFont = False
        end
        object Label3: TLabel
          Left = 378
          Top = 161
          Width = 27
          Height = 11
          Caption = 'Sound'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Small Fonts'
          Font.Style = []
          ParentFont = False
        end
        object lblPathNo: TLabel
          Left = 384
          Top = 28
          Width = 44
          Height = 14
          Caption = 'lblPathNo'
        end
        object Label37: TLabel
          Left = 149
          Top = 209
          Width = 56
          Height = 11
          Caption = 'Media Length'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'Small Fonts'
          Font.Style = []
          ParentFont = False
        end
        object DBText3: TDBText
          Left = 148
          Top = 223
          Width = 57
          Height = 17
          DataField = 'Media_Length'
          DataSource = dsPhotoTable
        end
        object Label38: TLabel
          Left = 101
          Top = 531
          Width = 58
          Height = 14
          Anchors = [akLeft, akBottom]
          Caption = 'Custom Key'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object lblLocation: TMemo
          Left = 160
          Top = 248
          Width = 269
          Height = 25
          BevelEdges = []
          BevelInner = bvNone
          BevelOuter = bvNone
          BorderStyle = bsNone
          ParentColor = True
          PopupMenu = puLocationInfo
          ReadOnly = True
          TabOrder = 20
        end
        object edtRootPath: TEdit
          Left = 58
          Top = 0
          Width = 343
          Height = 22
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          ReadOnly = True
          TabOrder = 9
          Text = '\\Newdell\newdell-e\My Pictures'
        end
        object dbFilePath: TDBEdit
          Left = 58
          Top = 24
          Width = 318
          Height = 22
          Anchors = [akLeft, akTop, akRight]
          DataField = 'File_Path'
          DataSource = dsFilePaths
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          ReadOnly = True
          TabOrder = 10
          OnKeyPress = dbFilePathKeyPress
        end
        object dbFileName: TDBEdit
          Left = 58
          Top = 50
          Width = 379
          Height = 22
          Anchors = [akLeft, akTop, akRight]
          DataField = 'File_Name'
          DataSource = dsPhotoTable
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          ReadOnly = True
          TabOrder = 11
          OnEnter = dbFileNameEnter
          OnKeyPress = dbFileNameKeyPress
        end
        object dbYear: TDBEdit
          Left = 5
          Top = 93
          Width = 70
          Height = 22
          DataField = 'Year'
          DataSource = dsPhotoTable
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
        end
        object dbMonth: TDBEdit
          Left = 88
          Top = 93
          Width = 43
          Height = 22
          DataField = 'Month'
          DataSource = dsPhotoTable
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          TabOrder = 3
        end
        object dbPhotoDate: TDBEdit
          Left = 213
          Top = 93
          Width = 97
          Height = 22
          DataField = 'PhotoDate'
          DataSource = dsPhotoTable
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          ReadOnly = True
          TabOrder = 5
        end
        object dbUpdated: TDBEdit
          Left = 6
          Top = 181
          Width = 101
          Height = 22
          DataField = 'Updated'
          DataSource = dsPhotoTable
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          TabOrder = 12
        end
        object dbAdded: TDBEdit
          Left = 111
          Top = 181
          Width = 101
          Height = 22
          DataField = 'Added'
          DataSource = dsPhotoTable
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          TabOrder = 13
        end
        object dbCopyCode: TDBEdit
          Left = 216
          Top = 179
          Width = 41
          Height = 22
          CharCase = ecUpperCase
          DataField = 'CopyR_ID'
          DataSource = dsPhotoTable
          TabOrder = 14
          OnMouseMove = dbCopyCodeMouseMove
        end
        object cbPrivate: TDBCheckBox
          Left = 260
          Top = 179
          Width = 33
          Height = 17
          DataField = 'Private'
          DataSource = dsPhotoTable
          TabOrder = 15
          ValueChecked = 'True'
          ValueUnchecked = 'False'
        end
        object dbKeyWords: TDBMemo
          Left = 64
          Top = 278
          Width = 371
          Height = 60
          Anchors = [akLeft, akTop, akRight]
          DataField = 'Key_Words'
          DataSource = dsPhotoTable
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          PopupMenu = puKeyWords
          TabOrder = 0
        end
        object dbComments: TDBMemo
          Left = 64
          Top = 344
          Width = 371
          Height = 175
          Anchors = [akLeft, akTop, akRight, akBottom]
          DataField = 'Comment'
          DataSource = dsPhotoTable
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          ScrollBars = ssVertical
          TabOrder = 1
        end
        object dbDay: TDBEdit
          Left = 142
          Top = 93
          Width = 43
          Height = 22
          DataField = 'Day'
          DataSource = dsPhotoTable
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          TabOrder = 4
        end
        object dbPhotoDateTime: TDBEdit
          Left = 5
          Top = 136
          Width = 380
          Height = 22
          DataField = 'PhotoDateTime'
          DataSource = dsPhotoTable
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          TabOrder = 8
          OnExit = dbPhotoDateTimeExit
        end
        object dbWasScanned: TDBCheckBox
          Left = 297
          Top = 179
          Width = 33
          Height = 17
          DataField = 'WasScanned'
          DataSource = dsPhotoTable
          TabOrder = 16
          ValueChecked = 'True'
          ValueUnchecked = 'False'
        end
        object dbHasSound: TDBCheckBox
          Left = 379
          Top = 179
          Width = 33
          Height = 17
          DataField = 'HasSound'
          DataSource = dsPhotoTable
          TabOrder = 17
          ValueChecked = 'True'
          ValueUnchecked = 'False'
        end
        object OvcDbNumericField1: TOvcDbNumericField
          Left = 316
          Top = 208
          Width = 113
          Height = 22
          DataField = 'Longitude'
          DataSource = dsPhotoTable
          FieldType = ftFloat
          CaretOvr.Shape = csBlock
          EFColors.Disabled.BackColor = clWindow
          EFColors.Disabled.TextColor = clGrayText
          EFColors.Error.BackColor = clRed
          EFColors.Error.TextColor = clBlack
          EFColors.Highlight.BackColor = clHighlight
          EFColors.Highlight.TextColor = clHighlightText
          Options = [efoCaretToEnd]
          PictureMask = '####.######'
          TabOrder = 19
          RangeHigh = {E175587FED2AB1ECFE7F}
          RangeLow = {E175587FED2AB1ECFEFF}
        end
        object OvcDbNumericField2: TOvcDbNumericField
          Left = 208
          Top = 208
          Width = 99
          Height = 22
          DataField = 'Latitude'
          DataSource = dsPhotoTable
          FieldType = ftFloat
          CaretOvr.Shape = csBlock
          EFColors.Disabled.BackColor = clWindow
          EFColors.Disabled.TextColor = clGrayText
          EFColors.Error.BackColor = clRed
          EFColors.Error.TextColor = clBlack
          EFColors.Highlight.BackColor = clHighlight
          EFColors.Highlight.TextColor = clHighlightText
          Options = [efoCaretToEnd]
          PictureMask = '####.######'
          TabOrder = 18
          RangeHigh = {E175587FED2AB1ECFE7F}
          RangeLow = {E175587FED2AB1ECFEFF}
        end
        object btnFillDate: TBitBtn
          Left = 320
          Top = 72
          Width = 75
          Height = 25
          Caption = 'Fill Date'
          TabOrder = 21
          OnClick = btnFillDateClick
        end
        object cbDateKind: TComboBox
          Left = 320
          Top = 104
          Width = 116
          Height = 22
          Style = csDropDownList
          ItemHeight = 14
          ItemIndex = 1
          TabOrder = 6
          Text = 'From EXIF'
          Items.Strings = (
            'Unknown'
            'From EXIF'
            'From FileName'
            'File Date Modified'
            'File Date Created'
            'File Date Last Access'
            'From Video/Audio Media Info')
        end
        object dbDateIsAGuess: TDBCheckBox
          Left = 212
          Top = 115
          Width = 97
          Height = 17
          Caption = 'Date is a guess'
          DataField = 'DateIsAGuess'
          DataSource = dsPhotoTable
          TabOrder = 7
          ValueChecked = 'True'
          ValueUnchecked = 'False'
        end
        object dbCustom_Key: TDBEdit
          Left = 164
          Top = 527
          Width = 76
          Height = 22
          Anchors = [akLeft, akBottom]
          DataField = 'CUSTOM_KEY'
          DataSource = dsPhotoTable
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          TabOrder = 22
        end
      end
      object cbUseFullSizeWindow: TCheckBox
        Left = 545
        Top = 3
        Width = 129
        Height = 17
        Caption = 'Use Full Size Window'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        OnClick = cbUseFullSizeWindowClick
      end
      object DBRadioGroup1: TDBRadioGroup
        Left = 995
        Top = 515
        Width = 284
        Height = 35
        Anchors = [akRight, akBottom]
        Caption = 'Interest'
        Columns = 5
        DataField = 'Rating'
        DataSource = dsPhotoTable
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        Items.Strings = (
          '&V Good'
          '&Good'
          '&Ok'
          'Poor'
          '&Terrible')
        ParentFont = False
        TabOrder = 4
        Values.Strings = (
          '2'
          '1'
          '0'
          '-1'
          '-2')
        OnClick = DBRadioGroup1Click
      end
      object btnAudioRecorder: TButton
        Left = 649
        Top = 524
        Width = 100
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Audio Recorder'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 5
        OnClick = btnAudioRecorderClick
      end
      object cbDisplayTextInFile: TCheckBox
        Left = 684
        Top = 3
        Width = 129
        Height = 17
        Caption = 'Display Text in File'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        OnClick = cbDisplayTextInFileClick
      end
      object cbShowScenes: TCheckBox
        Left = 808
        Top = 3
        Width = 97
        Height = 17
        Caption = 'Show Scenes'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 6
        OnClick = cbShowScenesClick
      end
      object btnOpenFile: TButton
        Left = 761
        Top = 524
        Width = 211
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Open File'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        TabOrder = 8
        OnClick = btnOpenFileClick
      end
      object cbAllowRotation: TCheckBox
        Left = 920
        Top = 3
        Width = 97
        Height = 17
        Caption = 'Allow Rotation'
        Checked = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        ParentFont = False
        State = cbChecked
        TabOrder = 7
        OnClick = cbAllowRotationClick
      end
    end
    object tabThumbNails: TTabSheet
      Caption = '&ThumbNails'
      ImageIndex = 3
      OnResize = tabThumbNailsResize
      object Label32: TLabel
        Left = 3
        Top = 12
        Width = 100
        Height = 14
        Caption = 'Max Photos to Show'
      end
      object lblStatus2: TLabel
        Left = 264
        Top = 11
        Width = 47
        Height = 14
        Caption = 'lblStatus2'
      end
      object ScrollBox1: TScrollBox
        Left = 0
        Top = 40
        Width = 733
        Height = 329
        HorzScrollBar.Visible = False
        BevelEdges = []
        BevelInner = bvNone
        BevelOuter = bvNone
        TabOrder = 0
      end
      object btnRefresh: TButton
        Left = 179
        Top = 5
        Width = 75
        Height = 25
        Caption = 'Refresh'
        TabOrder = 1
        OnClick = btnRefreshClick
      end
      object edtMaxPhotos: TEdit
        Left = 107
        Top = 8
        Width = 45
        Height = 22
        TabOrder = 2
        Text = '100'
        OnChange = edtMaxPhotosChange
      end
      object udMaxPhotos: TUpDown
        Left = 152
        Top = 8
        Width = 16
        Height = 22
        Associate = edtMaxPhotos
        Min = 1
        Max = 3000
        Position = 100
        TabOrder = 3
      end
    end
    object tabMover: TTabSheet
      Caption = 'Move Files'
      ImageIndex = 2
      OnResize = tabMoverResize
    end
  end
  object DBNavigator1: TDBNavigator
    Left = 1061
    Top = 588
    Width = 234
    Height = 25
    DataSource = dsPhotoTable
    VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast, nbDelete, nbEdit, nbPost, nbCancel, nbRefresh]
    Anchors = [akRight, akBottom]
    ParentShowHint = False
    ConfirmDelete = False
    ShowHint = True
    TabOrder = 1
  end
  object ovcKey: TOvcNumericField
    Left = 992
    Top = 590
    Width = 56
    Height = 20
    Cursor = crIBeam
    Hint = 'Go to Key'
    DataType = nftLongInt
    Anchors = [akRight, akBottom]
    AutoSize = False
    CaretOvr.Shape = csBlock
    EFColors.Disabled.BackColor = clWindow
    EFColors.Disabled.TextColor = clGrayText
    EFColors.Error.BackColor = clRed
    EFColors.Error.TextColor = clBlack
    EFColors.Highlight.BackColor = clHighlight
    EFColors.Highlight.TextColor = clHighlightText
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    Options = []
    ParentFont = False
    ParentShowHint = False
    PictureMask = '999999'
    ShowHint = True
    TabOrder = 2
    AfterExit = ovcKeyAfterExit
    RangeHigh = {7F969800000000000000}
    RangeLow = {00000000000000000000}
  end
  object btnLastRecord: TButton
    Left = 905
    Top = 588
    Width = 75
    Height = 24
    Anchors = [akRight, akBottom]
    Caption = 'Last Record'
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = btnLastRecordClick
  end
  object MainMenu1: TMainMenu
    Left = 136
    Top = 16
    object miMoveFile: TMenuItem
      Caption = 'File'
      object OpenSettings1: TMenuItem
        Caption = 'Open Settings...'
        OnClick = OpenSettings1Click
      end
      object NewProject1: TMenuItem
        Caption = 'New Settings...'
        Enabled = False
        Visible = False
      end
      object SaveProjectAs1: TMenuItem
        Caption = 'Save Settings As...'
        OnClick = SaveProjectAs1Click
      end
      object N6: TMenuItem
        Caption = '-'
        Visible = False
      end
      object AddPhoto1: TMenuItem
        Caption = 'Add Photo, Document, etc...'
        OnClick = AddPhoto1Click
      end
      object ReplaceCurrentPhoto1: TMenuItem
        Caption = 'Replace Current Photo...'
        OnClick = ReplaceCurrentPhoto1Click
      end
      object AddThumbnail1: TMenuItem
        Caption = 'Add Thumbnail'
        OnClick = AddThumbnail1Click
      end
      object AddFilepath1: TMenuItem
        Caption = 'Add Filepath...'
        OnClick = AddFilepath1Click
      end
      object ImportPhotos1: TMenuItem
        Caption = 'Scan for Photos/Documents...'
        OnClick = ImportPhotos1Click
      end
      object SaveAs1: TMenuItem
        Caption = 'Save Photo As...'
        OnClick = SaveAs1Click
      end
      object MoveFile1: TMenuItem
        Caption = 'Move File...'
        OnClick = miMoveFileClick
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object Print1: TMenuItem
        Caption = 'Print'
        OnClick = Print1Click
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object Options1: TMenuItem
        Caption = 'Options...'
        OnClick = Options1Click
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object Edit2: TMenuItem
      Caption = 'Edit'
      OnClick = Edit2Click
      object PhotoEditor1: TMenuItem
        Caption = 'Photo &Editor'
        ShortCut = 16453
        OnClick = PhotoEditor1Click
      end
      object miFindKeyWord: TMenuItem
        Caption = 'Find Key&word...'
        ShortCut = 16471
        OnClick = miFindKeyWordClick
      end
      object FindString1: TMenuItem
        Caption = 'Find &String...'
        ShortCut = 16454
        OnClick = FindString1Click
      end
      object FindAgain1: TMenuItem
        Caption = 'Find &Again'
        ShortCut = 16449
        OnClick = FindAgain1Click
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object EvaluateExpression2: TMenuItem
        Caption = 'Evaluate Expression...'
        OnClick = EvaluateExpression2Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object GoToKey1: TMenuItem
        Caption = 'Go To &Key...'
        ShortCut = 16459
        OnClick = GoToKey1Click
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object CopyfullFileName1: TMenuItem
        Caption = 'Copy full FileName...'
        OnClick = CopyfullFileName1Click
      end
      object RenameFile1: TMenuItem
        Caption = 'Rename File...'
        OnClick = RenameFile1Click
      end
      object ShowThumbnailsratherthanfullsizephoto1: TMenuItem
        Caption = 'Show Thumbnails rather than full size photo'
        OnClick = ShowThumbnailsratherthanfullsizephoto1Click
      end
    end
    object Navigate1: TMenuItem
      Caption = 'Navigate'
      object First1: TMenuItem
        Caption = 'First'
        ShortCut = 16420
        OnClick = First1Click
      end
      object Prev1: TMenuItem
        Caption = 'Prev'
        ShortCut = 33
        OnClick = Prev1Click
      end
      object Next1: TMenuItem
        Caption = 'Next'
        ShortCut = 34
        OnClick = Next1Click
      end
      object Last1: TMenuItem
        Caption = 'Last'
        ShortCut = 16419
        OnClick = Last1Click
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object Delete1: TMenuItem
        Caption = 'Delete'
        ShortCut = 16452
        OnClick = Delete1Click
      end
      object MarkNext1: TMenuItem
        Caption = 'Mark/Next'
        ShortCut = 16462
        OnClick = MarkNext1Click
      end
      object Post1: TMenuItem
        Caption = 'Post'
        ShortCut = 16467
        OnClick = Post1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Refresh1: TMenuItem
        Caption = 'Refresh'
        ShortCut = 24692
        OnClick = Refresh1Click
      end
      object Order1: TMenuItem
        Caption = 'Order'
        object FileName1: TMenuItem
          Caption = 'File Name'
          GroupIndex = 10
          RadioItem = True
          OnClick = FileName1Click
        end
        object PathName1: TMenuItem
          Caption = 'Path / Name'
          GroupIndex = 10
          RadioItem = True
          OnClick = PathName1Click
        end
        object PhotoDate1: TMenuItem
          Caption = 'Photo Date'
          GroupIndex = 10
          RadioItem = True
          object Ascending3: TMenuItem
            Caption = 'Ascending'
            OnClick = Ascending3Click
          end
          object Descending2: TMenuItem
            Caption = 'Descending'
            OnClick = Descending2Click
          end
        end
        object PhotoDateTime1: TMenuItem
          Caption = 'Photo Date/Time'
          GroupIndex = 10
          RadioItem = True
          OnClick = PhotoDateTime1Click
          object AScending4: TMenuItem
            Caption = 'Ascending'
            OnClick = AScending4Click
          end
          object Descending3: TMenuItem
            Caption = 'Descending'
            OnClick = Descending3Click
          end
        end
        object FileSize1: TMenuItem
          Caption = 'File Size'
          GroupIndex = 10
          RadioItem = True
          OnClick = FileSize1Click
        end
        object RecNo1: TMenuItem
          Caption = '(RecNo)'
          GroupIndex = 10
          RadioItem = True
          OnClick = RecNo1Click
        end
        object Updated1: TMenuItem
          Caption = 'Date Updated'
          GroupIndex = 10
          RadioItem = True
          OnClick = Updated1Click
        end
        object Added1: TMenuItem
          Caption = 'Date Added'
          GroupIndex = 10
          RadioItem = True
          OnClick = Added1Click
        end
        object Latitude1: TMenuItem
          Caption = 'Latitude'
          GroupIndex = 10
          RadioItem = True
          object Ascending1: TMenuItem
            Caption = 'Ascending'
            OnClick = Ascending1Click
          end
          object Descending1: TMenuItem
            Caption = 'Descending'
            OnClick = Descending1Click
          end
        end
        object Longitude1: TMenuItem
          Caption = 'Longitude'
          GroupIndex = 10
          RadioItem = True
          object Ascending2: TMenuItem
            Caption = 'Ascending'
            OnClick = Ascending2Click
          end
          object Decsending2: TMenuItem
            Caption = 'Decsending'
            OnClick = Decsencing1Click
          end
        end
        object MediaLength1: TMenuItem
          Caption = 'Media Length'
          GroupIndex = 10
          OnClick = MediaLength1Click
        end
        object CustomKey1: TMenuItem
          Caption = 'Custom Key'
          GroupIndex = 10
          OnClick = CustomKey1Click
        end
        object N12: TMenuItem
          Caption = '-'
          GroupIndex = 10
        end
        object DistancefromLocation1: TMenuItem
          Caption = 'Distance from Location...'
          GroupIndex = 10
          RadioItem = True
          OnClick = DistancefromLocation1Click
        end
        object InOrderbyLatLon1: TMenuItem
          Caption = 'In Order by Lat, Lon...'
          GroupIndex = 10
          RadioItem = True
          OnClick = InOrderbyLatLon1Click
        end
      end
    end
    object InsertText1: TMenuItem
      Caption = 'Insert Text'
      object InsertText2: TMenuItem
        Caption = 'Insert Text'
        object F1: TMenuItem
          Caption = 'F1'
          ShortCut = 112
          OnClick = F1Click
        end
        object F2: TMenuItem
          Caption = 'F2'
          ShortCut = 113
          OnClick = F2Click
        end
        object F3: TMenuItem
          Caption = 'F3'
          ShortCut = 114
          OnClick = F3Click
        end
        object F4: TMenuItem
          Caption = 'F4'
          ShortCut = 115
          OnClick = F4Click
        end
        object F5: TMenuItem
          Caption = 'F5'
          ShortCut = 116
          OnClick = F5Click
        end
        object F6: TMenuItem
          Caption = 'F6'
          ShortCut = 117
          OnClick = F6Click
        end
        object F7: TMenuItem
          Caption = 'F7'
          ShortCut = 118
          OnClick = F7Click
        end
        object F8: TMenuItem
          Caption = 'F8'
          ShortCut = 119
          OnClick = F8Click
        end
        object F9: TMenuItem
          Caption = 'F9'
          ShortCut = 120
          OnClick = F9Click
        end
        object F10: TMenuItem
          Caption = 'F10'
          ShortCut = 121
          OnClick = F10Click
        end
        object LastKeyWords1: TMenuItem
          Caption = '(Last Key Words)'
          ShortCut = 122
          OnClick = LastKeyWords1Click
        end
        object SameLocationasPreviousPost1: TMenuItem
          Caption = 'Same Location as Previous Post'
          ShortCut = 8314
          OnClick = SameLocationasPreviousPost1Click
        end
        object SameKeyWordsasLastPost1: TMenuItem
          Caption = 'Same Key Words as Last Post'
          ShortCut = 8315
          OnClick = SameKeyWordsasLastPost1Click
        end
        object RecalcKeyWords1: TMenuItem
          Caption = '(Recalc Key Words)'
          ShortCut = 16507
          OnClick = RecalcKeyWords1Click
        end
      end
      object InsertShiftText1: TMenuItem
        Caption = 'Insert Shift Text'
        object ShiftF1: TMenuItem
          Caption = 'Shift F1'
          ShortCut = 8304
          OnClick = ShiftF1Click
        end
        object ShiftF2: TMenuItem
          Caption = 'Shift F2'
          ShortCut = 8305
          OnClick = ShiftF2Click
        end
        object ShiftF3: TMenuItem
          Caption = 'Shift f3'
          ShortCut = 8306
          OnClick = ShiftF3Click
        end
        object ShiftF4: TMenuItem
          Caption = 'Shift f4'
          ShortCut = 8307
          OnClick = ShiftF4Click
        end
        object ShiftF5: TMenuItem
          Caption = 'Shift f5'
          ShortCut = 8308
          OnClick = ShiftF5Click
        end
        object ShiftF6: TMenuItem
          Caption = 'Shift f6'
          ShortCut = 8309
          OnClick = ShiftF6Click
        end
        object ShiftF7: TMenuItem
          Caption = 'Shift f7'
          ShortCut = 8310
          OnClick = ShiftF7Click
        end
        object ShiftF8: TMenuItem
          Caption = 'Shift f8'
          ShortCut = 8311
          OnClick = ShiftF8Click
        end
        object ShiftF9: TMenuItem
          Caption = 'Shift F9'
          ShortCut = 8312
          OnClick = ShiftF9Click
        end
        object ShiftF10: TMenuItem
          Caption = 'Shift f10'
          ShortCut = 8313
          OnClick = ShiftF10Click
        end
      end
      object InsertCtrlText1: TMenuItem
        Caption = 'Insert Ctrl Text'
        object CtrlF1: TMenuItem
          Caption = 'Ctrl F1'
          ShortCut = 16496
          OnClick = CtrlF1Click
        end
        object CtrlF2: TMenuItem
          Caption = 'Ctrl f2'
          ShortCut = 16497
          OnClick = CtrlF2Click
        end
        object CtrlF3: TMenuItem
          Caption = 'Ctrl f3'
          ShortCut = 16498
          OnClick = CtrlF3Click
        end
        object CtrlF4: TMenuItem
          Caption = 'Ctrl f4'
          ShortCut = 16499
          OnClick = CtrlF4Click
        end
        object CtrlF5: TMenuItem
          Caption = 'Ctrl f5'
          ShortCut = 16500
          OnClick = CtrlF5Click
        end
        object CtrlF6: TMenuItem
          Caption = 'Ctrl f6'
          ShortCut = 16501
          OnClick = CtrlF6Click
        end
        object CtrlF7: TMenuItem
          Caption = 'Ctrl f7'
          ShortCut = 16502
          OnClick = CtrlF7Click
        end
        object CtrlF8: TMenuItem
          Caption = 'Ctrl f8'
          ShortCut = 16503
          OnClick = CtrlF8Click
        end
        object CtrlF9: TMenuItem
          Caption = 'Ctrl f9'
          ShortCut = 16504
          OnClick = CtrlF9Click
        end
        object CtrlF10: TMenuItem
          Caption = 'Ctrl f10'
          ShortCut = 16505
          OnClick = CtrlF10Click
        end
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object PrintKeyAssignments1: TMenuItem
        Caption = 'Print Key Assignments'
        OnClick = PrintKeyAssignments1Click
      end
      object OpenFuncKeytxtforediting1: TMenuItem
        Caption = 'Browse FuncKey Values'
        OnClick = OpenFuncKeytxtforediting1Click
      end
    end
    object Image1: TMenuItem
      Caption = 'Image'
      object RotateRight902: TMenuItem
        Caption = 'Rotate &Right 90'#176
        ShortCut = 49234
        OnClick = RotateRight901Click
      end
      object RotateLeft901: TMenuItem
        Caption = 'Rotate &Left 90'#176
        ShortCut = 49228
        OnClick = RotateLeft01Click
      end
      object Rotate1802: TMenuItem
        Caption = 'Rotate 180'#176
        ShortCut = 49237
        OnClick = Rotate1801Click
      end
      object N15: TMenuItem
        Caption = '-'
      end
      object ShowEXIFMediaInfo1: TMenuItem
        Caption = 'Show EXIF/&Media Info...'
        OnClick = ShowEXIFInfo1Click
      end
    end
    object Utilities1: TMenuItem
      Caption = 'Utilities'
      object BuildInitialDB1: TMenuItem
        Caption = 'Build Initial DB'
        Visible = False
        OnClick = BuildInitialDB1Click
      end
      object Fill1: TMenuItem
        Caption = 'Fill'
        object FillYearMonthDayPhotoDate1: TMenuItem
          Caption = 'Fill Year, Month, Day, PhotoDate'
          object fromFileDate1: TMenuItem
            Caption = 'from File Date/Time'
            OnClick = fromFileDate1Click
          end
          object fromFileCreationTime1: TMenuItem
            Caption = 'from File Creation Date/Time'
            OnClick = fromFileCreationTime1Click
          end
          object fromExifDateTime1: TMenuItem
            Caption = 'from EXIF Date/Time'
            OnClick = fromExifDateTime1Click
          end
          object fromEnteredDateTime1: TMenuItem
            Caption = 'from Entered Date/Time'
            OnClick = fromEnteredDateTime1Click
          end
          object fromMediaDate1: TMenuItem
            Caption = 'from Media Date'
            OnClick = fromMediaDate1Click
          end
        end
        object FillPhotoDateTimefromPhotoDate1: TMenuItem
          Caption = 'Fill PhotoDateTime from PhotoDate'
          OnClick = FillPhotoDateTimefromPhotoDate1Click
        end
        object FillYearMonthDay1: TMenuItem
          Caption = 'Fill Year Month, Day'
          object FromPhotoDate1: TMenuItem
            Caption = 'from PhotoDate'
            OnClick = FromPhotoDate1Click
          end
          object FromPhotoDateTime1: TMenuItem
            Caption = 'From PhotoDateTime'
            OnClick = FromPhotoDateTime1Click
          end
        end
        object FillLocation1: TMenuItem
          Caption = 'Fill Location...'
          object fromGPXdatafiles1: TMenuItem
            Caption = 'from GPX data files...'
            OnClick = FillLocationinfofromGPXdatafiles1Click
          end
          object ClearLocationIDforselectedrecords1: TMenuItem
            Caption = 'Clear Location ID for selected records'
            OnClick = ClearLocationIDforselectedrecords1Click
          end
          object FillLocationFromEnteredData1: TMenuItem
            Caption = 'Fill Location From Entered Data'
            OnClick = FillLocationFromEnteredData1Click
          end
          object FillLocationfromEXIF1: TMenuItem
            Caption = 'Fill Location from EXIF'
            OnClick = FillLocationfromEXIF1Click
          end
          object FillLocationfromLatLon1: TMenuItem
            Caption = 'Fill Location from Lat/Lon...'
            OnClick = FillLocationfromLatLon1Click
          end
        end
        object FillFileSize1: TMenuItem
          Caption = 'Fill File Size'
          OnClick = FillFileSize1Click
        end
        object FillDateAddedfromFileDate1: TMenuItem
          Caption = 'Fill "Date Added" from File Date...'
          OnClick = FillDateAddedfromFileDate1Click
        end
        object FillLatitudeLongitudefromLocationID1: TMenuItem
          Caption = 'Fill Latitude, Longitude from Location ID'
          OnClick = FillLatitudeLongitudefromLocationID1Click
        end
        object UpdateHeightWidthforSelectedPhotos1: TMenuItem
          Caption = 'Fill Height/Width for Selected Photos/Videos'
          OnClick = UpdateHeightWidthforSelectedPhotos1Click
        end
        object FillTextInFilewithTextFromFile1: TMenuItem
          Caption = 'Fill TextInFile with Text From File'
          OnClick = FillTextInFilewithTextFromFile1Click
        end
        object FillWasScannedfromEXIF1: TMenuItem
          Caption = 'Fill WasScanned from EXIF'
          OnClick = FillWasScannedfromEXIF1Click
        end
        object FileDateIsAGuess1: TMenuItem
          Caption = 'Fill DateIsAGuess from FileName, KeyWords, Comment'
          OnClick = FileDateIsAGuess1Click
        end
        object FillMediaLengthfromFiles1: TMenuItem
          Caption = 'Fill Media Info from Files...'
          OnClick = FillMediaLengthfromFiles1Click
        end
      end
      object Browse1: TMenuItem
        Caption = 'Browse'
        OnClick = Browse1Click
        object BrowseFilePaths1: TMenuItem
          Caption = 'Browse File Paths...'
          OnClick = BrowseFilePaths1Click
        end
        object BrowseCopyrights1: TMenuItem
          Caption = 'Browse Copyrights...'
          OnClick = BrowseCopyrights1Click
        end
        object BrowseLocations1: TMenuItem
          Caption = 'Browse Locations...'
          OnClick = BrowseLocations1Click
        end
        object BrowseBoundaries1: TMenuItem
          Caption = 'Browse Boundaries...'
          OnClick = BrowseBoundaries1Click
        end
        object BrowseScenes1: TMenuItem
          Caption = 'Browse Scenes...'
          OnClick = BrowseScenes1Click
        end
        object BrowseFileNames1: TMenuItem
          Caption = 'Browse FileNames...'
          OnClick = BrowseFileNames1Click
        end
        object BrowseTracks1: TMenuItem
          Caption = 'Browse Tracks...'
          OnClick = BrowseTracks1Click
        end
        object BrowseLookups1: TMenuItem
          Caption = 'Browse Lookups...'
          object All1: TMenuItem
            Caption = 'All...'
            OnClick = All1Click
          end
          object Expressions1: TMenuItem
            Caption = 'Expressions...'
            OnClick = Expressions1Click
          end
          object Filters1: TMenuItem
            Caption = 'Filters...'
            OnClick = Filters1Click
          end
          object FunctionKeys1: TMenuItem
            Caption = 'Function Keys...'
            OnClick = FunctionKeys1Click
          end
          object BrowseNoiseWords1: TMenuItem
            Caption = 'Noise Words...'
            OnClick = BrowseNoiseWords1Click
          end
          object Synonyms1: TMenuItem
            Caption = 'Synonyms...'
            OnClick = Synonyms1Click
          end
        end
      end
      object CountSelectedRecords1: TMenuItem
        Caption = 'Count Selected Records'
        OnClick = CountSelectedRecords1Click
      end
      object CalculateFileSize1: TMenuItem
        Caption = 'Calculate Total Size for Selected Files...'
        OnClick = CalculateFileSize1Click
      end
      object AddKeyWordToSelectedPhotos1: TMenuItem
        Caption = 'Add Keyword to Selected Records...'
        OnClick = AddKeyWordToSelectedPhotos1Click
      end
      object ReplaceTextinSelected1: TMenuItem
        Caption = 'Replace Text in Selected Records...'
        OnClick = ReplaceTextinSelected1Click
      end
      object CopySelectedRecords1: TMenuItem
        Caption = 'Copy Selected Records...'
        OnClick = CopySelectedRecords1Click
      end
      object UpdateFieldinSelectedRecords1: TMenuItem
        Caption = 'Update Field in Selected Records...'
        OnClick = UpdateFieldinSelectedRecords1Click
      end
      object SetFileDateTimetoPhotoDateTimeforselectedrecords1: TMenuItem
        Caption = 'Set FileDateTime to PhotoDateTime for selected records...'
        OnClick = SetFileDateTimetoPhotoDateTimeforselectedrecords1Click
      end
      object UpdateThumbnailsforSelectedRecords1: TMenuItem
        Caption = 'Update Thumbnails for Selected Records'
        OnClick = UpdateThumbnailsforSelectedRecords1Click
      end
      object AdjustPhotoDatesTimes1: TMenuItem
        Caption = 'Adjust Photo Date(s)/Time(s)...'
        OnClick = AdjustPhotoDatesTimes1Click
      end
      object GenerateHTMLforSelectedFolders1: TMenuItem
        Caption = 'Generate HTML for Selected Folders...'
        OnClick = GenerateHTMLforSelectedFolders1Click
      end
      object UploadSelectedFiles1: TMenuItem
        Caption = 'Upload Selected Files...'
        Enabled = False
        Visible = False
        OnClick = UploadSelectedFiles1Click
      end
      object ExportSelectedRecords1: TMenuItem
        Caption = 'Export Selected Records...'
        OnClick = ExportSelectedRecords1Click
      end
      object ImportRecords1: TMenuItem
        Caption = 'Import Records...'
        OnClick = ImportRecords1Click
      end
      object Miscelaneous1: TMenuItem
        Caption = 'Miscellaneous'
        object DateTimeCalculation1: TMenuItem
          Caption = 'Date Time Calculation'
          OnClick = DateTimeCalculation1Click
        end
        object DeleteTrailingDates1: TMenuItem
          Caption = 'Delete Trailing Date(s)'
          OnClick = DeleteTrailingDates1Click
        end
        object RatePhotos1: TMenuItem
          Caption = 'Rate Photos'
          Enabled = False
          OnClick = RatePhotos1Click
        end
        object MatchSelectedFilestoFilesinFolder1: TMenuItem
          Caption = 'Match Selected Files to Files in Folder...'
          OnClick = MatchSelectedFilestoFilesinFolder1Click
        end
        object DeleteSelectedRecords1: TMenuItem
          Caption = 'Delete Selected Records...'
          OnClick = DeleteSelectedRecords1Click
        end
        object GenerateCSVFile1: TMenuItem
          Caption = 'Generate POI File...'
          OnClick = GenerateCSVFile1Click
        end
        object GenerateFilenamesList1: TMenuItem
          Caption = 'Generate ProShow Filenames List...'
          OnClick = GenerateFilenamesList1Click
        end
        object RenameFilesinFoldertoDateTimeTaken1: TMenuItem
          Caption = 'Rename Files in Folder to Date/Time Taken...'
          OnClick = RenameFilesinFoldertoDateTimeTaken1Click
        end
      end
    end
    object Reports1: TMenuItem
      Caption = 'Reports'
      object ScanforMissingFiles1: TMenuItem
        Caption = 'Scan for Missing Files'
        OnClick = ScanforMissingFiles1Click
      end
      object ScanforMissngFilesandUpdate1: TMenuItem
        Caption = 'Scan for Missng Files and Update'
        OnClick = ScanforMissngFilesandUpdate1Click
      end
      object ScanforFilesnotinDataBase1: TMenuItem
        Caption = 'Scan for Files not in DataBase...'
        OnClick = ScanforFilesnotinDataBase1Click
      end
      object ScanforDuplicates1: TMenuItem
        Caption = 'Scan for Duplicates'
        OnClick = ScanforDuplicates1Click
      end
      object ListSelectedRecords1: TMenuItem
        Caption = 'List Selected Records'
        OnClick = ListSelectedRecords1Click
      end
      object DuplicateFileSizeReport1: TMenuItem
        Caption = 'Duplicate FileSize Report'
        OnClick = DuplicateFileSizeReport1Click
      end
      object ScanSelectedforMissingThumbnail1: TMenuItem
        Caption = 'Scan Selected for Missing Thumbnail...'
        OnClick = ScanSelectedforMissingThumbnail1Click
      end
      object BrowseReportsFolder1: TMenuItem
        Caption = 'Browse Reports Folder...'
        OnClick = BrowseReportsFolder1Click
      end
    end
    object Help1: TMenuItem
      Caption = 'Help'
      object AboutPhotoDB1: TMenuItem
        Caption = 'About PhotoDB...'
        OnClick = AboutPhotoDB1Click
      end
    end
  end
  object dsPhotoTable: TDataSource
    Left = 472
    Top = 233
  end
  object dsFilePaths: TDataSource
    Left = 528
    Top = 252
  end
  object SaveDialog1: TSaveDialog
    Left = 190
    Top = 1
  end
  object puImage: TPopupMenu
    OnPopup = puImagePopup
    Left = 504
    Top = 187
    object Rotate1: TMenuItem
      Caption = 'Rotate'
      object RotateRight901: TMenuItem
        Caption = 'Rotate Right 90'#176
        OnClick = RotateRight901Click
      end
      object N14: TMenuItem
        Caption = 'Rotate Left 90'#176
        OnClick = RotateLeft01Click
      end
      object Rotate1801: TMenuItem
        Caption = 'Rotate 180'#176
        OnClick = Rotate1801Click
      end
    end
    object UseFullSizeWindow1: TMenuItem
      Caption = 'Use Full Size Window'
      OnClick = UseFullSizeWindow1Click
    end
    object ShowEXIFInfo1: TMenuItem
      Caption = 'Show EXIF/Media Info...'
      OnClick = ShowEXIFInfo1Click
    end
    object OpenMedia: TMenuItem
      Caption = 'Play Video'
      OnClick = OpenMediaClick
    end
    object PhotoEditor2: TMenuItem
      Caption = 'Photo Editor...'
      ShortCut = 16453
      OnClick = PhotoEditor2Click
    end
    object CreateThumbnail1: TMenuItem
      Caption = 'Create Thumbnail'
      OnClick = CreateThumbnail1Click
    end
    object EditThumbnail1: TMenuItem
      Caption = 'Edit Thumbnail'
      OnClick = EditThumbnail1Click
    end
    object AlternatePhotoEditor1: TMenuItem
      Caption = 'Alternate Photo Editor...'
      OnClick = AlternatePhotoEditor1Click
    end
    object PlayAudio1: TMenuItem
      Caption = 'Play Audio'
      OnClick = PlayAudio1Click
    end
    object Ratethisphoto2: TMenuItem
      Caption = 'Rate this photo, Next'
      object Excellent2: TMenuItem
        Caption = 'Excellent'
        RadioItem = True
        OnClick = Excellent2Click
      end
      object Good2: TMenuItem
        Caption = 'Good'
        RadioItem = True
        OnClick = Good2Click
      end
      object Ok2: TMenuItem
        Caption = 'Ok'
        RadioItem = True
        OnClick = Ok2Click
      end
      object Poor2: TMenuItem
        Caption = 'Poor'
        RadioItem = True
        OnClick = Poor2Click
      end
      object Terrible1: TMenuItem
        Caption = 'Terrible'
        RadioItem = True
        OnClick = Terrible1Click
      end
    end
    object N13: TMenuItem
      Caption = '-'
    end
  end
  object dsCopyRight: TDataSource
    Left = 564
    Top = 253
  end
  object puKeyWords: TPopupMenu
    AutoHotkeys = maManual
    AutoLineReduction = maManual
    OnPopup = puKeyWordsPopup
    Left = 375
    Top = 326
    object CaptureText1: TMenuItem
      Caption = 'Capture Text'
      object capF1: TMenuItem
        Caption = 'Capture to F1'
        ShortCut = 32880
        OnClick = capF1Click
      end
      object capF2: TMenuItem
        Caption = 'Capture to F2'
        ShortCut = 32881
        OnClick = capF2Click
      end
      object capF3: TMenuItem
        Caption = 'Capture to F3'
        ShortCut = 32882
        OnClick = capF3Click
      end
      object capF4: TMenuItem
        Caption = 'Capture to F4'
        ShortCut = 32883
        OnClick = capF4Click
      end
      object capF5: TMenuItem
        Caption = 'Capture to F5'
        ShortCut = 32884
        OnClick = capF5Click
      end
      object capF6: TMenuItem
        Caption = 'Capture to F6'
        ShortCut = 32885
        OnClick = capF6Click
      end
      object capF7: TMenuItem
        Caption = 'Capture to F7'
        ShortCut = 32886
        OnClick = capF7Click
      end
      object capF8: TMenuItem
        Caption = 'Capture to F8'
        ShortCut = 32887
        OnClick = capF8Click
      end
      object capF9: TMenuItem
        Caption = 'Capture to F9'
        ShortCut = 32888
        OnClick = capF9Click
      end
      object capF10: TMenuItem
        Caption = 'Capture to F10'
        ShortCut = 32889
        OnClick = capF10Click
      end
    end
    object ChangetoProperCase1: TMenuItem
      Caption = 'Change to Proper Case'
      ShortCut = 16464
      OnClick = ChangetoProperCase1Click
    end
    object InsertLocationatCursor1: TMenuItem
      Caption = 'Insert Location at Cursor'
      ShortCut = 16460
      OnClick = InsertLocationatCursor1Click
    end
    object N7: TMenuItem
      Caption = '-'
    end
    object Cut1: TMenuItem
      Caption = 'Cut'
      ShortCut = 16472
      OnClick = Cut1Click
    end
    object Copy1: TMenuItem
      Caption = 'Copy'
      ShortCut = 16451
      OnClick = Copy1Click
    end
    object Paste1: TMenuItem
      Caption = 'Paste'
      ShortCut = 16470
      OnClick = Paste1Click
    end
    object UnDo1: TMenuItem
      Caption = 'UnDo'
      ShortCut = 16474
      OnClick = UnDo1Click
    end
  end
  object puLocationInfo: TPopupMenu
    Left = 400
    Top = 277
    object AddLocationInfo1: TMenuItem
      Caption = 'Add Location Info'
      object CreatefromScratch1: TMenuItem
        Caption = 'Create from Scratch...'
        OnClick = CreatefromScratch1Click
      end
      object BasedonDateTime1: TMenuItem
        Caption = 'Based on Date/Time...'
        OnClick = BasedonDateTime1Click
      end
      object Bynumber1: TMenuItem
        Caption = 'By number...'
        OnClick = Bynumber1Click
      end
      object FromEXIFInfo1: TMenuItem
        Caption = 'From EXIF Info...'
        OnClick = FromEXIFInfo1Click
      end
    end
    object EditLocationInfo1: TMenuItem
      Caption = 'Edit Location Info'
      OnClick = EditLocationInfo1Click
    end
    object DeleteLocationInfo1: TMenuItem
      Caption = 'Delete Location Info'
      OnClick = DeleteLocationInfo1Click
    end
    object CopyLocationInfo1: TMenuItem
      Caption = 'Copy Location Info'
      OnClick = CopyLocationInfo1Click
    end
    object PasteLocationInfo1: TMenuItem
      Caption = 'Paste Location Info'
      ShortCut = 16470
      OnClick = PasteLocationInfo1Click
    end
    object FindLocation1: TMenuItem
      Caption = 'Find Location'
      object ByDescription1: TMenuItem
        Caption = 'By Description...'
        OnClick = FindLocation1Click
      end
      object ByLocation1: TMenuItem
        Caption = 'By Location...'
        OnClick = ByLocation1Click
      end
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object CopyLatitude1: TMenuItem
      Caption = 'Copy Latitude'
      ShortCut = 16468
      OnClick = CopyLatitude1Click
    end
    object CopyLongitude1: TMenuItem
      Caption = 'Copy Longitude'
      ShortCut = 16455
      OnClick = CopyLongitude1Click
    end
  end
  object puThumbNail: TPopupMenu
    OnPopup = puThumbNailPopup
    Left = 576
    Top = 53
    object SelectThisPhoto1: TMenuItem
      Caption = 'Select This Photo'
      OnClick = SelectThisPhoto1Click
    end
    object Ratethisphoto1: TMenuItem
      Caption = 'Rate this photo'
      OnClick = Ratethisphoto1Click
      object Excellent1: TMenuItem
        Caption = 'Excellent'
        RadioItem = True
        OnClick = Excellent1Click
      end
      object Good1: TMenuItem
        Caption = 'Good'
        RadioItem = True
        OnClick = Good1Click
      end
      object Ok1: TMenuItem
        Caption = 'Ok'
        RadioItem = True
        OnClick = Ok1Click
      end
      object Fair1: TMenuItem
        Caption = 'Poor'
        RadioItem = True
        OnClick = Fair1Click
      end
      object Poor1: TMenuItem
        Caption = 'Terrible'
        RadioItem = True
        OnClick = Poor1Click
      end
    end
    object EditPhoto1: TMenuItem
      Caption = 'Edit Photo...'
      OnClick = EditPhoto1Click
    end
    object DeletePhoto1: TMenuItem
      Caption = 'Delete Photo'
      OnClick = DeletePhoto1Click
    end
  end
  object PopupMenu2: TPopupMenu
    Left = 436
    Top = 392
    object IsDHD1: TMenuItem
      Caption = 'Is DHD'
      OnClick = IsDHD1Click
    end
  end
  object DropFileTarget1: TDropFileTarget
    DragTypes = [dtCopy]
    OnDrop = DropFileTarget1Drop
    Target = Panel1
    OptimizedMove = True
    Left = 328
    Top = 16
  end
end
