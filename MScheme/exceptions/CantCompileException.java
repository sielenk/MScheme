package MScheme.exceptions;

import MScheme.values.Value;


public class CantCompileException
    extends CompilationException
{
    public CantCompileException(Value cause)
    { super(cause); }
}

