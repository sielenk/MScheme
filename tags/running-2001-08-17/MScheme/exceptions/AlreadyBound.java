package MScheme.exceptions;

import MScheme.values.Symbol;


public class AlreadyBound
    extends CompileError
{
    public AlreadyBound(Symbol cause)
    { super(cause); }
}

