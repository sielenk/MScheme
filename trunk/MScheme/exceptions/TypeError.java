package MScheme.exceptions;

import MScheme.Value;


public class TypeError
    extends SchemeException
{
    public final static String id
        = "$Id$";

    public TypeError(Value cause)
    { super(cause); }
}
