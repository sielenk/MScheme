package MScheme.exceptions;

import MScheme.expressions.SExpr;

public class SImproperListException extends SException
{
    public SImproperListException(SExpr sexpr)
    {
        super(sexpr);
    }
}
