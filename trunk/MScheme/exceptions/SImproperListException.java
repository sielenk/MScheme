package MScheme.exceptions;

import MScheme.expressions.SExpr;

public class SImproperListException extends SException
{
    SImproperListException(SExpr sexpr)
    {
        super(sexpr);
    }
}
