package MScheme.exceptions;

import MScheme.expressions.SExpr;

public class SExpectedFunctionException extends SException
{
    public SExpectedFunctionException(SExpr sexpr)
    {
        super(sexpr);
    }
}
