package MScheme.exceptions;

import MScheme.values.Value;


public class ImmutableException
    extends RuntimeError
{
    public ImmutableException(Value cause)
    { super(cause); }
}

