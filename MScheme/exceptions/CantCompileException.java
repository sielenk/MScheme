package MScheme.exceptions;

import MScheme.values.Value;


public class CantCompileException
    extends SyntaxException
{
    public CantCompileException(Value cause)
    { super(cause); }
}

