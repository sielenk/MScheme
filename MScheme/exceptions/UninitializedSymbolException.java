package MScheme.exceptions;

import MScheme.values.Symbol;


public class UninitializedSymbolException
    extends RuntimeError
{
    public UninitializedSymbolException(Symbol cause)
    {
        super(cause);
    }
}
