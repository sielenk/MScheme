package MScheme.exceptions;

import MScheme.Value;


public class CompileError
    extends SchemeException
{
    public CompileError(Value cause)
    { super(cause); }
}
