package MScheme.exceptions;

import MScheme.Value;


public abstract class TypeError
    extends SchemeException
{
    public final static String id
        = "$Id$";

    public TypeError(Value cause, String message)
    { super(cause, message + " expected"); }
}
