package MScheme.exceptions;

import MScheme.values.Value;


public class OpenException
    extends SchemeIOException
{
    public OpenException(Value cause)
    { super(cause); }
}

