package MScheme.exceptions;

import MScheme.Value;


public class RuntimeError
    extends SchemeException
{
    public RuntimeError(Value cause)
    { super(cause); }
}
