package MScheme.exceptions;

import MScheme.Value;


public final class PairExpected
    extends TypeError
{
    public final static String id
        = "$Id$";

    public PairExpected(Value cause)
    { super(cause, "pair"); }
}
