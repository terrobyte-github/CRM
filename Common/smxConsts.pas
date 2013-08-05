unit smxConsts;

interface

const
  { XML }
  cRootNodeName = 'Root';
  cCellNodeName = 'Cell';
  cTypeNodeName = 'Type';
  cFontNodeName = 'Font';
  cKitNodeName = 'Kit';
  cItemNodeName = 'Item';
  cXMLDocVersion = '1.0';
  cXMLDocEncoding = 'UTF-8';
  cXMLDocTextDef =
    '<?xml version="' + cXMLDocVersion + '"' +
    ' encoding="' + cXMLDocEncoding + '"?>' +
    '<' + cRootNodeName + '>' +
    '</' + cRootNodeName + '>';
  cSuffixTextFieldName = 'Text';

  { Global objects }
  cApplicationHandle = 0;
  cStorageManager = 1;
  cLibraryManager = 2;
  cDatabaseManager = 3;
  cFormManager = 4;
  cImageListManager = 5;
  //cMainConnection = 6;

resourcestring
  rsActionError = 'A ClassName %s %s error';
  rsAssignError = 'Cannot assign a ClassName %s to a ClassName %s';
  rsCfgActionError = 'A ClassName %s of configuration with ID %d %s error';
  rsCellActionError = 'A ClassName %s of cell %s error';
  rsCellIDActionError = 'A ClassName %s of cell with ID %d %s error';
  rsListItemNotFound = 'Item ''%s'' not found';
  rsListItemClassError = 'A ClassName %s invalid for ClassName %s';

implementation

end.
