package MScheme.exceptions;

import MScheme.values.Value;


public class SymbolExpectedException
    extends ExpectedException
{
    public SymbolExpectedException(Value cause)
    { super(cause); }
}

