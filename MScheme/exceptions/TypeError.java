package MScheme.exceptions;

import MScheme.values.Value;


public class TypeError
    extends SchemeException
{
    public TypeError(Value cause)
    { super(cause); }
}
