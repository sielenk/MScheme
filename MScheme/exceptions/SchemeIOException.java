package MScheme.exceptions;

import MScheme.values.Value;


public class SchemeIOException
    extends SchemeException
{
    public SchemeIOException(Value cause)
    { super(cause); }
}

