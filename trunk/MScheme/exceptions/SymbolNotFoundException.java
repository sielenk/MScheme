package MScheme.exceptions;

import MScheme.Value;


public class SymbolNotFoundException
    extends CompileError
{
    public SymbolNotFoundException(Value cause)
    { super(cause); }
}

