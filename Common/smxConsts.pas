unit smxConsts;

interface

const
  //SFileConnectName = 'conn.ini';
  //SProcLibInfoName = 'LibInfo';
  //SFuncNewResourceName = 'NewResource';
  //SCloseProgMessage = 'Закрыть программу?';
  //SFileProjectName = 'proj.dat';
  SFileConfigurationName = '..\Cfg\conf.ini'; //'conf.ini'; //'Cfg\cfg.ini';
  SXMLDocTextDef = '<?xml version="1.0"?><Root></Root>';
  SFilterNameSuffix = 'Text';

resourcestring
  { DBInterface }
  SDBIntfDatabaseInvalid = 'Class of database is invalid type';
  SDBIntfDataSetInvalid = 'Class of dataset is invalid type';
  SDBIntfFieldInvalid = 'Class of field is invalid type';
  SDBIntfParamInvalid = 'Class of parameter is invalid type';
  SDBIntfConnectFailed = 'Connection to a database is failed';
  SDBIntfDataSetUnknown = 'Type of dataset unknown';

  { Cfg }
  SCfgBuildError = 'A ClassName %s of configuration with ID %d build error';
  //SCfgFinalizeError = 'Configuration finalize error';
  //SCfgInitializeError = 'Configuration initialize error';
  SCfgLoadError = 'A ClassName %s of configuration with ID %d load error';
  SCfgSaveError = 'A ClassName %s of configuration with ID %d save error';
  SCfgReadError = 'A ClassName %s of configuration with ID %d read error';
  SCfgWriteError = 'A ClassName %s of configuration with ID %d write error';
  SCfgRemoveError = 'A ClassName %s of configuration with ID %d remove error';

  { Cell }
  SCellBuildError = 'A cell with ID %d build error';
  SCellRequestPerformError = 'A ClassName %s of Request with ID %d perform error';

  { LibManager }
  //SLibManagerFreeError = 'Library free error';
  //SLibManagerLoadError = 'Library load error';

  { Algorithm }
  SAlgExecuteError = 'Algorithm execute error';

  { Kit }
  SKitItemNotFound = 'In a ClassName %s item with name %s is not found';
  SKitItemExist = 'In a ClassName %s item with name %s already exist';
  SKitItemClassTypeError = 'Cannot set a ClassName %s to a ClassName %s';
  SKitItemAssignError = 'Cannot assign a ClassName %s to a ClassName %s';

implementation

end.
