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
import MScheme.values.List;

import MScheme.exceptions.CompileError;
import MScheme.exceptions.TypeError;


final class If
    extends Syntax
{
    final static Syntax INSTANCE = new If();
    
    private If()
    { super(Arity.inRange(2, 3)); }


    protected Code checkedTranslate(
        StaticEnvironment syntax,
        List              arguments
    ) throws CompileError, TypeError
    {
        Value flag    = arguments.getHead();
        Value onTrue  = arguments.getTail().getHead();
        Value onFalse =
            arguments.getTail().getTail().isEmpty()
            ? ScmBoolean.createFalse()
            : arguments.getTail().getTail().getHead();

        return new Selection(
            flag.   getCode(syntax),
            onTrue. getCode(syntax),
            onFalse.getCode(syntax)
        );
    }
}
