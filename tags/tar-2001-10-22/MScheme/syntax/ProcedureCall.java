package MScheme.syntax;

import MScheme.Syntax;
import MScheme.Code;

import MScheme.values.List;

import MScheme.util.Arity;

import MScheme.code.CodeList;
import MScheme.code.Application;

import MScheme.environment.StaticEnvironment;

import MScheme.exceptions.SchemeException;


public final class ProcedureCall
    implements Syntax
{
    public final static String id
        = "$Id$";


    private final Code _head;

    private ProcedureCall(Code head)
    {
        _head = head;
    }

    public static ProcedureCall create(Code head)
    {
        return new ProcedureCall(head);
    }

    public Code translate(
        StaticEnvironment compilationEnv,
        List              arguments
    ) throws SchemeException
    {
        return Application.create(
            CodeList.prepend(
                _head,
                arguments.getCodeList(compilationEnv)
            )
        );
    }
}
