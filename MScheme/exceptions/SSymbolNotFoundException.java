package MScheme.exceptions;

import MScheme.expressions.SSymbol;

public class SSymbolNotFoundException extends SException 
{
    SSymbolNotFoundException(SSymbol symbol)
    {
        super(symbol);
	}
}

