package MScheme.environment;


import MScheme.machine.Values;

import MScheme.expressions.SExpr;
import MScheme.expressions.SSymbol;

import MScheme.exceptions.SSymbolNotFoundException;
import MScheme.exceptions.SDuplicateSymbolException;


public interface Environment
{
    // *** Envrionment access ************************************************

    public Environment     getEmpty    (              );

    public Environment     getParent   (              );
    public Environment     newChild    (              );
    public EnvironmentStub newChildStub(Values symbols)
        throws SDuplicateSymbolException;

    // *** Envrionment access ************************************************

    public boolean defined(SSymbol symbol);

    public void    define (SSymbol symbol, SExpr value);

    public void set(SSymbol symbol, SExpr value)
        throws SSymbolNotFoundException;

    public SExpr get(SSymbol symbol)
        throws SSymbolNotFoundException;

    // ***********************************************************************
}
