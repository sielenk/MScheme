package MScheme.exceptions;

import MScheme.Value;


public final class CloseException
    extends SchemeIOException
{
    public final static String id
        = "$Id$";

    public CloseException(Value cause)
    { super(cause); }
}
