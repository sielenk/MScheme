package MScheme.exceptions;

import MScheme.values.Value;


public class RuntimeError
    extends SchemeException
{
    public RuntimeError(Value cause)
    { super(cause); }
}
