package MScheme.exceptions;

import MScheme.Value;


public class StringExpected
    extends TypeError
{
    public final static String id
        = "$Id$";

    public StringExpected(Value cause)
    { super(cause); }
}
