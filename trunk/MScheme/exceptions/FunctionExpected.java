package MScheme.exceptions;

import MScheme.Value;


public final class FunctionExpected
    extends TypeError
{
    public final static String id
        = "$Id$";

    public FunctionExpected(Value cause)
    { super(cause, "function"); }
}
