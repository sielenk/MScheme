package MScheme.exceptions;

import MScheme.expressions.SExpr;

public class SExpectedFunctionException extends SException
{
    SExpectedFunctionException(SExpr sexpr)
    {
        super(sexpr);
    }
}
