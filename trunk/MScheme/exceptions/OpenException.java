package MScheme.exceptions;

import MScheme.Value;


public class OpenException
    extends SchemeIOException
{
    public OpenException(Value cause)
    { super(cause); }
}

