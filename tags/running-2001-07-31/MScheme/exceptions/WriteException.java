package MScheme.exceptions;

import MScheme.values.Value;


public class WriteException
    extends SchemeIOException
{
    public WriteException(Value cause)
    { super(cause); }
}

