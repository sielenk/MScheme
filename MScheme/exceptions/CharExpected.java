package MScheme.exceptions;

import MScheme.Value;


public class CharExpected
    extends TypeError
{
    public final static String id
        = "$Id$";

    public CharExpected(Value cause)
    { super(cause); }
}
