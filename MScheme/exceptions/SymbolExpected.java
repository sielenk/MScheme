package MScheme.exceptions;

import MScheme.values.Value;


public class SymbolExpected
    extends TypeError
{
    public SymbolExpected(Value cause)
    { super(cause); }
}

