package MScheme.exceptions;

import MScheme.values.Symbol;


public class UnexpectedSyntax
    extends CompileError
{
    public UnexpectedSyntax(Symbol cause)
    { super(cause); }
}

