package MScheme.exceptions;

import MScheme.Value;


public class VectorExpected
    extends TypeError
{
    public final static String id
        = "$Id$";

    public VectorExpected(Value cause)
    { super(cause); }
}
