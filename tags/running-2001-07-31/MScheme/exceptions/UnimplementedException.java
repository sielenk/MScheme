package MScheme.exceptions;

import MScheme.values.Value;


public class UnimplementedException
    extends SchemeException
{
    public UnimplementedException(Value cause)
    { super(cause); }
}

