package MScheme.exceptions;

import MScheme.Value;


public final class WriteException
    extends SchemeIOException
{
    public final static String id
        = "$Id$";

    public WriteException(Value cause)
    { super(cause); }
}

