package MScheme.exceptions;

import MScheme.Value;


public class SymbolExpected
    extends TypeError
{
    public SymbolExpected(Value cause)
    { super(cause); }
}

