package MScheme.syntax;

import MScheme.Value;
import MScheme.Code;
import MScheme.Syntax;

import MScheme.util.Arity;
import MScheme.code.Assignment;
import MScheme.environment.StaticEnvironment;
import MScheme.environment.Reference;
import MScheme.values.List;
import MScheme.values.Symbol;

import MScheme.exceptions.*;


abstract class Assign
    extends Syntax
{
    protected Assign()
    { super(Arity.atLeast(2)); }

    abstract protected Reference getReference(
        StaticEnvironment syntax,
        Symbol            symbol
    ) throws CompileError;

    protected Code checkedTranslate(
        StaticEnvironment syntax,
	    int               len,
        List              arguments
    ) throws CompileError, TypeError
    {
        if (len > 2) {
	        arityError(arguments);
	    }
	    
        Symbol symbol = arguments.getHead().toSymbol();
        Value  value  = arguments.getTail().getHead();

        return new Assignment(
            getReference(syntax, symbol),
            value.getCode(syntax)
        );
    }
}
