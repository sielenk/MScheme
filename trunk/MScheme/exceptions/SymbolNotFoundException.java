package MScheme.exceptions;

import MScheme.values.Value;


public class SymbolNotFoundException
    extends SyntaxException
{
    public SymbolNotFoundException(Value cause)
    {
        super(cause);
    }
}

