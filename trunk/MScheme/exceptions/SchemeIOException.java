package MScheme.exceptions;

import MScheme.Value;


public class SchemeIOException
    extends RuntimeError
{
    public SchemeIOException(Value cause)
    { super(cause); }
}

