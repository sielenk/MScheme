package MScheme.code;

import MScheme.util.Arity;
import MScheme.machine.Machine;
import MScheme.environment.*;
import MScheme.exceptions.*;
import MScheme.functions.*;
import MScheme.values.*;


// *** if ***

final class Selector
    extends UnaryFunction
{
    private final Code _onTrue;
    private final Code _onFalse;

    Selector(Code onTrue, Code onFalse)
    { _onTrue = onTrue; _onFalse = onFalse; }

    protected Code checkedCall(Machine machine, Value flag)
    { return flag.isFalse() ? _onFalse : _onTrue; }
}

final class IfToken
    extends Token
{
    final static Token INSTANCE = new IfToken();
    
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
            ? ValueFactory.createFalse()
            : arguments.getTail().getTail().getHead();
            
        Function selector = new Selector(
            onTrue .getCode(syntax),
            onFalse.getCode(syntax)
        );

        return CodeList.create(
            selector.getCode(syntax),
            flag    .getCode(syntax)
        );
    }
}

