package MScheme.exceptions;


import MScheme.expressions.SExpr;


public class SImproperFormalsException extends SException
{
    public SImproperFormalsException(SExpr sexpr)
    {
        super(sexpr);
    }
}
