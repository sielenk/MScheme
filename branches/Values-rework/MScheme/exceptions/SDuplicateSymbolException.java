package MScheme.exceptions;

import MScheme.expressions.SList;

public class SDuplicateSymbolException extends SException
{
    public SDuplicateSymbolException(SList symbols)
    {
        super(symbols);
    }
}
