package MScheme.exceptions;

import MScheme.values.Value;


public class CompileError
    extends SchemeException
{
    public CompileError(Value cause)
    { super(cause); }
}
