package MScheme.exceptions;

import MScheme.Value;


public class ListExpected
    extends TypeError
{
    public final static String id
        = "$Id$";

    public ListExpected(Value cause)
    { super(cause); }
}
