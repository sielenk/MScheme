package MScheme.exceptions;

import MScheme.values.Value;


public class CloseException
    extends SchemeIOException
{
    public CloseException(Value cause)
    { super(cause); }
}

