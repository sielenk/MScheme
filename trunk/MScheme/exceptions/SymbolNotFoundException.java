package MScheme.exceptions;

import MScheme.values.Value;


public class SymbolNotFoundException
    extends CompileError
{
    public SymbolNotFoundException(Value cause)
    {
        super(cause);
    }
}

