unit smxConsts;

interface

const
  cFileConfigurationName = '..\Cfg\conf.ini';
  cDefIntValue = -99;
  cRootNodeName = 'Root';
  cCellNodeName = 'Cell';
  cFontNodeName = 'Font';
  cItemNodeName = 'Item';
  cSlaveNodeName = 'Slave';
  cXMLDocVersion = '1.0';
  cXMLDocEncoding = 'UTF-8';
  cXMLDocTextDef =
    '<?xml version="' + cXMLDocVersion + '"' +
    ' encoding="' + cXMLDocEncoding + '"?>' +
    '<' + cRootNodeName + '>' +
    '</' + cRootNodeName + '>';
  cSuffixFilterText = 'Text';  

resourcestring
  rsActionError = 'A ClassName %s %s error';
  rsAssignError = 'Cannot assign a ClassName %s to a ClassName %s';
  rsCfgActionError = 'A ClassName %s of configuration with ID %d %s error';
  rsCellActionError = 'A ClassName %s of cell %s error';
  rsListItemNotFound = 'Item ''%s'' not found';

implementation

end.
