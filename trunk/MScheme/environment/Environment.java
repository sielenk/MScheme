package MScheme.environment;


import MScheme.machine.Values;

import MScheme.expressions.SExpr;
import MScheme.expressions.SSymbol;

import MScheme.exceptions.SSymbolNotFoundException;


public interface Environment
{
    // *** instance access ***************************************************

    public int getLevel();
    public int getSize ();

    // *** Envrionment access ************************************************

    public Environment     getParent   (              );
    public Environment     newChild    (              );
    public EnvironmentStub newChildStub(Values symbols);

    // *** Envrionment access ************************************************

    public boolean defined(SSymbol symbol);

    public void    define (SSymbol symbol, SExpr value);

    public void set(SSymbol symbol, SExpr value)
        throws SSymbolNotFoundException;

    public SExpr get(SSymbol symbol)
        throws SSymbolNotFoundException;

    // ***********************************************************************
}
