package MScheme.exceptions;

import MScheme.Value;


public class ImmutableException
    extends RuntimeError
{
    public ImmutableException(Value cause)
    { super(cause); }
}

