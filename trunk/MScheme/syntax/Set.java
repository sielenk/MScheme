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

import MScheme.exceptions.SchemeException;


final class Set
            extends Syntax
{
    public final static String id
    = "$Id$";


    final static Syntax INSTANCE = new Set();

    private Set()
    {
        super(Arity.exactly(2));
    }

    protected Code checkedTranslate(
        StaticEnvironment compilationEnv,
        List              arguments
    ) throws SchemeException
    {
        Symbol symbol = arguments.getHead().toSymbol();
        Value  value  = arguments.getTail().getHead();

        return translate(
                   symbol.getReference(compilationEnv),
                   value .getCode     (compilationEnv)
               );
    }

    static Code translate(
        Reference reference,
        Code      code
    )
    {
        return Assignment.create(
                   reference,
                   code
               );
    }
}
