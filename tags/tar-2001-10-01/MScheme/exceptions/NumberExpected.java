package MScheme.exceptions;

import MScheme.Value;


public final class NumberExpected
            extends TypeError
{
    public final static String id
    = "$Id$";

    public NumberExpected(Value cause)
    {
        super(cause, "number");
    }
}
