package MScheme.exceptions;

import MScheme.expressions.SExpr;

public class SCantEvaluateException extends SException
{
    public SCantEvaluateException(SExpr sexpr)
    {
        super(sexpr);
    }
}
