package MScheme.exceptions;

import MScheme.expressions.SSymbol;

public class SSymbolNotFoundException extends SException
{
    public SSymbolNotFoundException(SSymbol symbol)
    {
        super(symbol);
    }
}
