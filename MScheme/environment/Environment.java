package MScheme.environment;

import MScheme.expressions.SExpr;
import MScheme.expressions.SSymbol;
import MScheme.exceptions.SSymbolNotFoundException;

public class Environment {

    SExpr lookup(SSymbol symbol)
        throws SSymbolNotFoundException
    {
        throw new SSymbolNotFoundException(symbol);
    }
}
