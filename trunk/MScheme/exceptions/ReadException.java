package MScheme.exceptions;

import MScheme.Value;


public class ReadException
    extends SchemeIOException
{
    public ReadException(Value cause)
    { super(cause); }
}

