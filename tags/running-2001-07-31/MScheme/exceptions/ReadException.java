package MScheme.exceptions;

import MScheme.values.Value;


public class ReadException
    extends SchemeIOException
{
    public ReadException(Value cause)
    { super(cause); }
}

