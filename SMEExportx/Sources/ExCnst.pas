{ SMExport suite
  SMExport english string resources.

  Copyright (C) 1998-2005, written by Shkolnik Mike, Scalabium
  E-Mail:  smexport@scalabium.com
  WEB: http://www.scalabium.com
}
unit ExCnst;

interface

const
  strPropertyError: string = 'Property %s is not specified.';
  strIsNotDBGrid: string = 'Assigned DBGrid (%s) is not a supported type';
  strExportFinished: string = 'The %s file was successfully generated with exported data.';
  strExportError: string = 'Error of data export.';
  strExportOk: string = 'Data export was successfully completed.';
  strFileCreateError: string = 'Could not create the %s file.';
  strHTMLTitle: string = 'Data exported by SMExport generator';
  strFilePrompt: string = 'File %s already exists. Do you want to replace it?';

  strExporting: string = 'Exporting...';
  strLoadError: string = 'Can''t create a process dialog';
  strSetupDlgCaption: string = 'Data Export Setup';
  strBtnOk: string = '&OK';
  strBtnCancel: string = '&Cancel';
  strBtnBack: string = '< &Back';
  strBtnNext: string = '&Next >';
  strExecute: string = '&Execute';
  strExportCanceled: string = 'Data export was canceled by user.';
  strSMEWizardCaption: string = 'Export Wizard';
  strStepDisplay: string = 'Step %d of %d';
  strWizardAbort: string = 'Wizard is incomplete. If you quit the Wizard now, the new export will not be created. Abort wizard?';
  strExportStart: string = 'Do you want to start the export process?';
  strFileOverwrite: string = '%s file exists. To overwrite this file?';

  strLblStep1: string = 'This wizard allows you to specify details of how to export your data. Which export format would you like?';
  strLblStep2: string = 'Specify custom options for data exporting';
  strLblStep3: string = 'Select the appropriate delimiter to separate your fields.';
  strLblStep4: string = 'You can define any custom data formats.';
  strLblStep5: string = 'You can define custom properties for the exported columns.';
  strLblStep6: string = 'You can add a custom header and/or footer.';
  strLblStep6A: string = 'Customize settings for page';
  strLblStep7: string = 'You can select desired layout of exported data';
  strLblStep8: string = 'That''s all of the information the wizard needs to export your data.';

  strHeader: string = 'Header';
  strFooter: string = 'Footer';

  strTableType: string = ' Table type ';
  strFormat1: string = 'Paradox file (*.db)';
  strFormat2: string = 'DBase file (*.dbf)';
  strFormat3: string = 'Text file (*.txt)';
  strFormat4: string = 'HTML file (*.htm)';
  strFormat5: string = 'Excel spreadsheet (*.xls)';
  strFormat6: string = 'Excel file (*.xls)';
  strFormat7: string = 'Word file (*.doc)';
  strFormat8: string = 'SYLK (Symbolic Link) (*.slk)';
  strFormat9: string = 'DIF (Data Interchange Format) (*.dif)';
  strFormatA: string = 'Lotus 1-2-3 file (*.wk1)';
  strFormatB: string = 'QuattroPro file (*.wq1)';
  strFormatC: string = 'SQL script file (*.sql)';
  strFormatD: string = 'XML file (*.xml)';
  strFormatE: string = 'MS Access database (*.mdb)';
  strFormatF: string = 'MS Windows clipboard';
  strFormatG: string = 'Rich Text format (*.rtf)';
  strFormatH: string = 'SPSS format (*.sav)';
  strFormatI: string = 'Adobe Acrobat Document (*.pdf)';
  strFormatJ: string = 'LDAP DataInterchangeFormat (*.ldif)';
  strFormatK: string = 'ADO connection';

  strFileOrigin: string = 'File Origin:';
  strSelectedOnly: string = 'Selected records only';
  strAddCaption: string = 'Include column titles';
  strAddTitle: string = 'Put field titles on the first row';
  strAddBlankRow: string = 'add a blank row after field names';
  strBlankIfZero: string = 'Blank if zero';
  strActionAfterExport: string = 'Action after exporting';
  strAENone: string = 'None';
  strAEOpenView: string = 'open for file view';
  strAEEMail: string = 'e-mail with file attachment';

  strSeparatorType: string = ' Field delimiter ';
  strDelimiter1: string = 'Tab';
  strDelimiter2: string = 'Semicolon';
  strDelimiter3: string = 'Comma';
  strDelimiter4: string = 'Space';
  strDelimiter5: string = 'Other symbol:';
  strFixed: string = 'Fixed column length';
  strText1: string = '&Delimited - Characters such as commas or tabs separate each field';
  strText2: string = 'Fixed &Width - Fields are aligned in columns with spaces between each field';

  strFileName: string = 'Export to a File:';
  strSaveDlgTitle: string = 'Export to';
  strAllFiles: string = 'All files (*.*)|*.*';
  strTableName: string = 'Table name:';

  SgbTitle: string = ' Title ';
  SgbData: string = ' Data ';
  STitleCaption: string = 'Caption:';
  STitleAlignment: string = 'Alignment:';
  STitleColor: string = 'Background:';
  STitleFont: string = 'Font:';
  SWidth: string = 'Width:';
  SWidthFix: string = 'characters';
  SAlignLeft: string = 'left';
  SAlignRight: string = 'right';
  SAlignCenter: string = 'center';

  strESpecs: string = 'Specifications...';
  strESpecCaption: string = 'Export specification';
  strESpecPrompt: string = 'List of available specifications:';
  strELoad: string = 'Load';
  strESave: string = 'Save';
  strEDelete: string = 'Delete';
  strEClose: string = 'Close';
  strEDeleteSpecPrompt: string = 'Delete import specification %s?';

  strERecordSeparator: string = 'Record separator:';
  strETextQualifier: string = 'Text qualifier:';
  strENone: string = 'none';

  strERowsPerFile: string = 'Records per each file';

  strPreviewLayout: string = 'Preview';
  strLayout: string = ' Layout ';
  strColumnar: string = 'columnar';
  strReversedColumnar: string = 'reversed columnar';
  strTabularForm: string = 'tabular form';
  strColorStyle: string = 'Color Style:';

  strDateNumFormat: string = ' Dates, Times and Numbers ';
  strMDY: string = 'MDY';
  strDMY: string = 'DMY';
  strYMD: string = 'YMD';
  strYDM: string = 'YDM';
  strDYM: string = 'DYM';
  strMYD: string = 'MYD';
  strDateOrder: string = 'Date &Order:';
  strDateDelimiter: string = 'Date De&limiter:';
  strTimeDelimiter: string = 'Time Deli&miter:';
  strFourDigitYears: string = 'Four Digit &Years';
  strLeadingZerosInDate: string = 'Leading &Zeros in Dates';
  strDecimalSymbol: string = 'Decimal Sym&bol:';
  strThousandSymbol: string = '&Thousand Separator:';
  strCurrencyString: string = '&Currency Symbol:';

  strLogicalValues: string = 'Logical values:';
  strTrue: string = 'True';
  strFalse: string = 'False';

  strField: string = 'Field';
  strTextFieldName: string = 'Text';

  strSelectAll: string = 'Select &All';
  strUnSelectAll: string = '&Unselect All';
  strRevertSelection: string = '&Revert selection';

  strMergeFile: string = 'Merge existing file';

  strStepHeader1: string = 'File Format';
  strStepHeader2: string = 'Data Origin';
  strStepHeader3: string = 'Text Settings';
  strStepHeader4: string = 'Data Formats';
  strStepHeader5: string = 'Columns';
  strStepHeader6: string = 'Header and Footer';
  strStepHeader6A: string = 'Page Setup';
  strStepHeader7: string = 'Layout';
  strStepHeader8: string = 'File Name';

  strExportColorsFonts: string = 'export data and colors/fonts';
  strExportDataOnly: string = 'export data only';

  strExportOriginalValueTypes: string = 'export the original value types';
  strSheetName: string = 'Sheet/generator name:';

  {page setup}
  strOrientation: string = ' Orientation ';
  strDefault: string = 'Default';
  strPortrait: string = 'Portrait';
  strLandscape: string = 'Landscape';

  strMargins: string = ' Default &Margins ';
  strLeft: string = 'Left';
  strTop: string = 'Top';
  strRight: string = 'Right';
  strBottom: string = 'Bottom';

  strInsertSysVar: string = 'Insert system variable';
  strSVRowNo: string = 'row number';
  strSVColNo: string = 'column number';
  strSVRecCount: string = 'total row count';
  strSVErrCount: string = 'error number';
  strSVSheetName: string = 'sheet name (generator)';

implementation

end.