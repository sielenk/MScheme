package MScheme.exceptions;

import MScheme.Value;


public final class OpenException
            extends SchemeIOException
{
    public final static String id
    = "$Id$";

    public OpenException(Value cause)
    {
        super(cause);
    }
}
