package MScheme.exceptions;

import MScheme.Value;


public final class VectorExpected
            extends TypeError
{
    public final static String id
    = "$Id$";

    public VectorExpected(Value cause)
    {
        super(cause, "vector");
    }
}
