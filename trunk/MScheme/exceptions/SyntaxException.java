package MScheme.exceptions;

import MScheme.values.Value;


public class SyntaxException
    extends CompileError
{
    public SyntaxException(Value cause)
    { super(cause); }
}

