package MScheme.syntax;

import MScheme.util.Arity;
import MScheme.machine.Machine;
import MScheme.code.Code;
import MScheme.code.CodeList;
import MScheme.environment.StaticEnvironment;
import MScheme.code.CompiledSelector;
import MScheme.values.Value;
import MScheme.values.Function;
import MScheme.values.ScmBoolean;
import MScheme.values.List;

import MScheme.exceptions.CompileError;
import MScheme.exceptions.TypeError;


final class IfToken
    extends Syntax
{
    final static Syntax INSTANCE = new IfToken();
    
    private IfToken()
    { super(Arity.inRange(2, 3)); }


    protected Code checkedTranslate(
        StaticEnvironment syntax,
	    int               len,
        List              arguments
    ) throws CompileError, TypeError
    {
        Value flag    = arguments.getHead();
        Value onTrue  = arguments.getTail().getHead();
        Value onFalse =
            (len == 2)
            ? ScmBoolean.createFalse()
            : arguments.getTail().getTail().getHead();

        return new CompiledSelector(
            flag.   getCode(syntax),
            onTrue. getCode(syntax),
            onFalse.getCode(syntax)
        );
    }
}

