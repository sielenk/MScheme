package MScheme.exceptions;

import MScheme.Value;


public final class CharExpected
    extends TypeError
{
    public final static String id
        = "$Id$";

    public CharExpected(Value cause)
    { super(cause, "character"); }
}
