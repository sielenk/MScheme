package MScheme.exceptions;

import MScheme.values.Symbol;


public class UninitializedSymbolException
    extends RuntimeError
{
    public final static String id
        = "$Id$";

    public UninitializedSymbolException(Symbol cause)
    {
        super(cause);
    }
}
