package MScheme.exceptions;

import MScheme.Value;


public class DuplicateSymbolException
    extends CompileError
{
    public DuplicateSymbolException(Value cause)
    {
        super(cause);
    }
}

