package MScheme.exceptions;

import MScheme.Value;


public class OpenException
    extends SchemeIOException
{
    public final static String id
        = "$Id$";

    public OpenException(Value cause)
    { super(cause); }
}

