package MScheme.exceptions;

import MScheme.values.Value;


public class CompilationException
    extends SchemeException
{
    public CompilationException(Value cause)
    { super(cause); }
}

