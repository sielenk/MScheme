package MScheme.exceptions;

import MScheme.Value;


public class CloseException
    extends SchemeIOException
{
    public CloseException(Value cause)
    { super(cause); }
}

