package MScheme.exceptions;

import MScheme.values.Value;


public class DuplicateSymbolException
    extends SyntaxException
{
    public DuplicateSymbolException(Value cause)
    {
        super(cause);
    }
}

