package MScheme.exceptions;

import MScheme.Value;


public class WriteException
    extends SchemeIOException
{
    public final static String id
        = "$Id$";

    public WriteException(Value cause)
    { super(cause); }
}

