package MScheme.exceptions;

import MScheme.values.Symbol;


public class UninitializedSymbolException
    extends SchemeException
{
    public UninitializedSymbolException(Symbol cause)
    {
        super(cause);
    }
}
