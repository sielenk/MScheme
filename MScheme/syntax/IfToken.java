package MScheme.syntax;

import MScheme.util.Arity;
import MScheme.machine.Machine;
import MScheme.code.Code;
import MScheme.code.CodeList;
import MScheme.environment.StaticEnvironment;
import MScheme.code.CompiledSelector;
import MScheme.values.Value;
import MScheme.values.Function;
import MScheme.values.SchemeBoolean;
import MScheme.values.List;

import MScheme.exceptions.SchemeException;


final class IfToken
    extends Syntax
{
    final static Syntax INSTANCE = new IfToken();
    
    private IfToken()
    { super(Arity.inRange(2, 3)); }


    protected Code checkedTransform(
        StaticEnvironment syntax,
        List              arguments
    ) throws SchemeException
    {
        Value flag    = arguments.getHead();
        Value onTrue  = arguments.getTail().getHead();
        Value onFalse =
            (arguments.getLength() == 2)
            ? SchemeBoolean.createFalse()
            : arguments.getTail().getTail().getHead();

        return new CompiledSelector(
            flag.   getCode(syntax),
            onTrue. getCode(syntax),
            onFalse.getCode(syntax)
        );
    }
}

