package MScheme.exceptions;

import MScheme.Value;


public class CloseException
    extends SchemeIOException
{
    public final static String id
        = "$Id$";

    public CloseException(Value cause)
    { super(cause); }
}

