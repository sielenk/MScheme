package MScheme.syntax;

import MScheme.Value;
import MScheme.Code;
import MScheme.Syntax;

import MScheme.util.Arity;
import MScheme.machine.Machine;
import MScheme.code.CodeList;
import MScheme.environment.StaticEnvironment;
import MScheme.code.Selection;
import MScheme.values.Function;
import MScheme.values.ScmBoolean;
import MScheme.List;

import MScheme.exceptions.CompileError;
import MScheme.exceptions.TypeError;


final class If
    extends Syntax
{
    public final static String id
        = "$Id$";


    final static Syntax INSTANCE = new If();
    
    private If()
    { super(Arity.inRange(2, 3)); }


    protected Code checkedTranslate(
        StaticEnvironment compilationEnv,
        List              arguments
    ) throws CompileError, TypeError
    {
        Value flag    = arguments.getHead();
        Value onTrue  = arguments.getTail().getHead();
        Value onFalse =
            arguments.getTail().getTail().isEmpty()
            ? ScmBoolean.createFalse()
            : arguments.getTail().getTail().getHead();

        return Selection.create(
            flag.   getCode(compilationEnv),
            onTrue. getCode(compilationEnv),
            onFalse.getCode(compilationEnv)
        );
    }
}
