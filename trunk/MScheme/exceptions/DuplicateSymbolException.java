package MScheme.exceptions;

import MScheme.values.Value;


public class DuplicateSymbolException
    extends CompileError
{
    public DuplicateSymbolException(Value cause)
    {
        super(cause);
    }
}

