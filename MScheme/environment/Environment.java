package MScheme.environment;


import MScheme.util.Values;

import MScheme.expressions.SExpr;
import MScheme.expressions.SSymbol;

import MScheme.exceptions.SSymbolNotFoundException;
import MScheme.exceptions.SDuplicateSymbolException;


public interface Environment
{
    // *** Envrionment access ************************************************

    Environment     getParent   (              );
    Environment     newChild    (              );
    EnvironmentStub newChildStub(Values symbols)
        throws SDuplicateSymbolException;

    // *** Envrionment access ************************************************

    boolean defined(SSymbol symbol);

    void    define (SSymbol symbol, SExpr value);

    void set(SSymbol symbol, SExpr value)
        throws SSymbolNotFoundException;

    SExpr get(SSymbol symbol)
        throws SSymbolNotFoundException;

    // ***********************************************************************
}
