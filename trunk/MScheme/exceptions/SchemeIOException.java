package MScheme.exceptions;

import MScheme.values.Value;


public class SchemeIOException
    extends RuntimeError
{
    public SchemeIOException(Value cause)
    { super(cause); }
}

