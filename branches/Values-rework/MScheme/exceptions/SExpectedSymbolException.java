package MScheme.exceptions;

import MScheme.expressions.SExpr;

public class SExpectedSymbolException extends SException
{
    public SExpectedSymbolException(SExpr sexpr)
    {
        super(sexpr);
    }
}
