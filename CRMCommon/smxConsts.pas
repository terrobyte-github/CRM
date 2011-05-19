unit smxConsts;

interface

const
  SFileConnectName = 'conn.ini';
  SProcLibInfoName = 'LibInfo';
  SFuncLoadResourceName = 'LoadResource';
  SCloseProgMessage = 'Закрыть программу?';
  SFileProjectName = 'proj.dat';

resourcestring
  { DBInterface }
  SDBIntfDatabaseInvalid = 'Class of database is invalid type';
  SDBIntfDataSetInvalid = 'Class of dataset is invalid type';
  SDBIntfFieldInvalid = 'Class of field is invalid type';
  SDBIntfParamInvalid = 'Class of parameter is invalid type';
  SDBIntfConnectFailed = 'Connection to a database is failed';

  { Cfg }
  SCfgBuildError = 'Configuration build error';
  SCfgFinalizeError = 'Configuration finalize error';
  SCfgInitializeError = 'Configuration initialize error';

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
