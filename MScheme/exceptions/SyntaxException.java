package MScheme.exceptions;

import MScheme.values.Value;


public class SyntaxException
    extends CompilationException
{
    public SyntaxException(Value cause)
    { super(cause); }
}

