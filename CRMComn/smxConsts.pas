unit smxConsts;

interface

const
  FileConnectParams = 'conn.ini';
  CloseProgMessage = 'Закрыть программу?';

resourcestring
  { DBInterface }
  SDBIntfDatabaseInvalid = 'Class of database is invalid type';
  SDBIntfDataSetInvalid = 'Class of dataset is invalid type';
  SDBIntfFieldInvalid = 'Class of field is invalid type';
  SDBIntfParamInvalid = 'Class of parameter is invalid type';
  SDBIntfConnectFailed = 'Database connect failed';

  { Cfg }
  SCfgFinalizeError = 'Configuration finalize error';
  SCfgInitializeError = 'Configuration initialize error';

  { Cell }
  SCellBuildError = 'Cell build error';
  SCellRequestPerformError = 'Request perform error';

  { LibManager }
  SLibManagerFreeError = 'Library free error';
  SLibManagerLoadError = 'Library load error';

  { Algorithm }
  SAlgExecuteError = 'Algorithm execute error';

  { TargetRequest }
  STargetRequestDatabaseError = 'Database is not assign';

implementation

end.
