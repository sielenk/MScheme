package MScheme.syntax;

import MScheme.util.Arity;

import MScheme.Code;
import MScheme.Value;
import MScheme.Syntax;

import MScheme.code.Assignment;
import MScheme.environment.StaticEnvironment;
import MScheme.environment.Reference;
import MScheme.values.List;
import MScheme.values.Symbol;

import MScheme.exceptions.CompileError;
import MScheme.exceptions.TypeError;


final class Set
    extends Syntax
{
    final static Syntax INSTANCE = new Set();
    
    private Set()
    { super(Arity.exactly(2)); }

    protected Code checkedTranslate(
        StaticEnvironment syntax,
        List              arguments
    ) throws CompileError, TypeError
    {
        Symbol symbol = arguments.getHead().toSymbol();
        Value  value  = arguments.getTail().getHead();

        return translate(
            symbol.getReference(syntax),
            value .getCode     (syntax)
        );
    }

    static Code translate(
        Reference reference,
        Code      code
    )
    {
        return new Assignment(
            reference,
            code
        );
    }
}
