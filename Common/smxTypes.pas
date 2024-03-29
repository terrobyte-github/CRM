{**************************************}
{                                      }
{            SalesMan v1.0             }
{             Base types               }
{                                      }
{          Copyright (c) 2010          }
{          Polyakov �leksandr          }
{                                      }
{**************************************}

unit smxTypes;

interface

uses
  DB;

type
  TsmxGenerationMode = (gmFunction, gmCOM, gmClass);

  TsmxProjectConnection = record
    ProjectName: ShortString;
    Generation: TsmxGenerationMode;
    LibraryName: ShortString;
    FuncOrClassNameOrProgID: ShortString;
    IsWindowsAuthorization: Boolean;
    DatabaseName: ShortString;
    DriverName: ShortString;
    Params: ShortString;
    IsUseUser: Boolean;
    UserName: ShortString;
    Password: ShortString;
  end;

  TsmxFuncDatabaseCLSID = function: TGUID;

  TsmxChangeMode = (cmReplace, cmAdd, cmUnique);

  TsmxOperationMode = (omManual, omAutomatic);

  TsmxGridOption = (goColLines, goRowLines, goRowSelect, goShowHeader,
    goEditing, goColResize);

  TsmxGridOptions = set of TsmxGridOption;

  TsmxRequestType = (rtSelect, rtDelete, rtInsert, rtUpdate);

  TsmxModifyRequest = rtDelete .. rtUpdate;

  TsmxColumnOption = (coEditing, coResize);

  TsmxColumnOptions = set of TsmxColumnOption;

  TsmxTreeOption = (toColLines, toRowLines, toRowSelect, toShowHeader,
    toEditing, toTreeLines, toColResize);

  TsmxTreeOptions = set of TsmxTreeOption;

  TsmxFilterOption = (foEditing, foResize);

  TsmxFilterOptions = set of TsmxFilterOption;

  TsmxFormPosition = (fpDesigned, fpScreenCenter, fpOwnerFormCenter);

  TsmxFormBorder = (fbNone, fbDialog, fbResize);

  TsmxFormOption = (foFrameForm, foFreeOnClose);

  TsmxFormOptions = set of TsmxFormOption;

  TsmxPageManagerStyle = (pmsTab, pmsFlat);

  TsmxPagePosition = (ppTop, ppButtom, ppLeft, ppRight);

  TsmxMenuItemStyle = (misPoint, misDivider);

  TsmxToolItemStyle = (tisButton, tisCheck, tisDivider);

  TsmxStatusItemStyle = (sisText, sisDraw);

  TsmxEditorType = (etNone, etString, etPickString, etButtonString);

  TsmxCellState = (csInitialized, csFinalized, csEventParam, csDesigning);

  TsmxCellStates = set of TsmxCellState;

  //TsmxCellStyle = (csNotRecieveCfg, csIsDesigning);

  //TsmxCellStyles = set of TsmxCellStyle;

  TsmxDataSetType = (dstQuery, dstStoredProc);

  TsmxDataType = TFieldType;

  TsmxDataSense = (dsGeneral, dsKey, dsValue, dsResult, dsMessage,
    dsForeignKey);

  TsmxParamType = TParamType;

  TsmxDataLocation = (dlAssigned, dlStorageParam, dlCellParam,
    dlParentCellParam, dlEventParam, dlParentEventParam, dlWorkCell,
    dlParentWorkCell, dlFilterDesk, dlParentFilterDesk, dlGrid, dlParentGrid,
    dlTree, dlParentTree);

  TsmxLocateOptions = TLocateOptions;

  TsmxPerformanceMode = (pmOpen, pmExecute);

  TsmxGUIDArray = array of TGUID;

  TsmxAlgorithmCellOption = (acoProperties, acoEvents);

  TsmxAlgorithmCellOptions = set of TsmxAlgorithmCellOption;

implementation

end.
