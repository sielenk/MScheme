package MScheme.exceptions;

import MScheme.Value;


public final class StringExpected
            extends TypeError
{
    public final static String id
    = "$Id$";

    public StringExpected(Value cause)
    {
        super(cause, "string");
    }
}
