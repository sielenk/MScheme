package MScheme.exceptions;

import MScheme.Value;


public class SchemeIOException
            extends RuntimeError
{
    public final static String id
    = "$Id$";

    public SchemeIOException(Value cause)
    {
        super(cause);
    }
}

