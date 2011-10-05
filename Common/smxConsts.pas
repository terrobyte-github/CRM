unit smxConsts;

interface

const
  //SFileConnectName = 'conn.ini';
  //SProcLibInfoName = 'LibInfo';
  //SFuncNewResourceName = 'NewResource';
  //SCloseProgMessage = '������� ���������?';
  //SFileProjectName = 'proj.dat';
  SFileConfigurationName = '..\Cfg\conf.ini'; //'conf.ini'; //'Cfg\cfg.ini';
  SXMLDocTextDef = '<?xml version="1.0"?><Root></Root>';

resourcestring
  { DBInterface }
  SDBIntfDatabaseInvalid = 'Class of database is invalid type';
  SDBIntfDataSetInvalid = 'Class of dataset is invalid type';
  SDBIntfFieldInvalid = 'Class of field is invalid type';
  SDBIntfParamInvalid = 'Class of parameter is invalid type';
  SDBIntfConnectFailed = 'Connection to a database is failed';
  SDBIntfDataSetUnknown = 'Type of dataset unknown';

  { Cfg }
  SCfgBuildError = 'Configuration build error';
  SCfgFinalizeError = 'Configuration finalize error';
  SCfgInitializeError = 'Configuration initialize error';
  SCfgLoadError = 'ClassName %s of configuration with ID %d load error';
  SCfgSaveError = 'ClassName %s of configuration with ID %d save error';
  SCfgReadError = 'ClassName %s of configuration with ID %d read error';
  SCfgWriteError = 'ClassName %s of configuration with ID %d write error';

  { Cell }
  SCellBuildError = 'Cell build error';
  SCellRequestPerformError = 'Request perform Error';

  { LibManager }
  //SLibManagerFreeError = 'Library free error';
  //SLibManagerLoadError = 'Library load error';

  { Algorithm }
  SAlgExecuteError = 'Algorithm execute error';

  { Kit }
  SKitParamNotFound = 'The parameter is not found';
  SKitParamExists = 'The parameter already exists';

implementation

end.
