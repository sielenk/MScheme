package MScheme.exceptions;

import MScheme.values.Value;


public class ImmutableException
    extends SchemeException
{
    public ImmutableException(Value cause)
    { super(cause); }
}

