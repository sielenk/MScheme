package MScheme.exceptions;

import MScheme.Value;


public class ReadException
            extends SchemeIOException
{
    public final static String id
    = "$Id$";

    public ReadException(Value cause)
    {
        super(cause);
    }
}

