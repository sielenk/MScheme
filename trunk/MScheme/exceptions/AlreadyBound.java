package MScheme.exceptions;

import MScheme.values.Symbol;


public final class AlreadyBound
            extends CompileError
{
    public final static String id
    = "$Id$";

    public AlreadyBound(Symbol cause)
    {
        super(cause, "symbol already bound");
    }
}
