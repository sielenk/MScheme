package MScheme.exceptions;

import MScheme.Value;


public class CantCompileException
    extends CompileError
{
    public CantCompileException(Value cause)
    { super(cause); }
}

