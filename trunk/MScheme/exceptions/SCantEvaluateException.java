package MScheme.exceptions;

import MScheme.expressions.SExpr;

public class SCantEvaluateException extends SException
{
    SCantEvaluateException(SExpr sexpr)
    {
        super(sexpr);
    }
}
