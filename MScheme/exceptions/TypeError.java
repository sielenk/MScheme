package MScheme.exceptions;

import MScheme.Value;


public class TypeError
    extends SchemeException
{
    public TypeError(Value cause)
    { super(cause); }
}
