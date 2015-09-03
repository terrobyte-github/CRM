{**************************************}
{                                      }
{            SalesMan v1.0             }
{            Base constants            }
{                                      }
{          Copyright (c) 2010          }
{          Polyakov Àleksandr          }
{                                      }
{**************************************}

unit smxConsts;

interface

uses
  xmldom;

const
  { XML }
  cRootNodeName = 'Root';
  cCellNodeName = 'Cell';
  cSlaveNodeName = 'Slave';
  cTypeNodeName = 'Type';
  cFontNodeName = 'Font';
  cKitNodeName = 'Kit';
  cItemNodeName = 'Item';
  cCfgIDAttributeName = 'CfgID';
  cNameAttributeName = 'Name';
  cClassNameAttributeName = 'ClassName';
  cXMLDocVersion = '1.0';
  cXMLDocEncoding = 'UTF-8';
  cXMLDocTextDef =
    '<?' + xmldom.SXML +
    ' ' + xmldom.SVersion + '="' + cXMLDocVersion + '"' +
    ' ' + xmldom.SEncoding + '="' + cXMLDocEncoding + '"?>' +
    '<' + cRootNodeName + '/>';

  { Consts }
  cSuffixTextFieldName = 'Text';
  cDelimiterObjAndMethName = '.';
  cPrefixClassName = 'Tsmx';

  { Global objects }
  cApplicationHandle = 0;
  cStorageManager = 1;
  cLibraryManager = 2;
  cDatabaseManager = 3;
  cFormManager = 4;
  cImageListManager = 5;
  cClassTypeManager = 6;

  { Constructor }
  cFormObjectPropsID = -100;
  cFormSlaveListID = -101;
  cFormCellViewID = -102;

resourcestring
  rsActionError = 'A ClassName %s %s error';
  rsAssignError = 'Cannot assign a ClassName %s to a ClassName %s';
  rsCfgActionError = 'A ClassName %s of configuration with ID %d %s error';
  rsCfgActionErrorM = 'A ClassName %s of configuration with ID %d %s error'#13#10'%s';
  rsCellActionError = 'A ClassName %s of cell %s error';
  rsCellIDActionError = 'A ClassName %s of cell with ID %d %s error';
  rsCellIDActionErrorM = 'A ClassName %s of cell with ID %d %s error'#13#10'%s';
  rsListItemNotFound = 'Item ''%s'' not found';
  rsListItemClassError = 'A ClassName %s invalid for ClassName %s';
  rsListActionError = 'A ClassName %s of item %s error';

implementation

end.
