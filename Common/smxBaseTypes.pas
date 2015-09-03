{**************************************}
{                                      }
{            SalesMan v1.0             }
{             Base types               }
{                                      }
{          Copyright (c) 2010          }
{          Polyakov Àleksandr          }
{                                      }
{**************************************}

unit smxBaseTypes;

interface

uses
  smxBaseClasses;

type
  TsmxComponentEvent = procedure(Sender: TsmxComponent) of object;

  TsmxProcExecuteEvent = procedure(Sender: TsmxComponent);

implementation

end.
