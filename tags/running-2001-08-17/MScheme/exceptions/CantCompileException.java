package MScheme.exceptions;

import MScheme.values.Value;


public class CantCompileException
    extends CompileError
{
    public CantCompileException(Value cause)
    { super(cause); }
}

