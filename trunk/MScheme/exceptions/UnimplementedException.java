package MScheme.exceptions;

import MScheme.Value;


public class UnimplementedException
    extends SchemeException
{
    public UnimplementedException(Value cause)
    { super(cause); }
}

