package MScheme.exceptions;

import MScheme.Value;


public class WriteException
    extends SchemeIOException
{
    public WriteException(Value cause)
    { super(cause); }
}

