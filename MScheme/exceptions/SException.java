package MScheme.exceptions;

import MScheme.expressions.SExpr;

public abstract class SException extends Exception
{
    private SExpr _sexpr;

    protected SException(
        SExpr sexpr
    ) {
        super();
        _sexpr = sexpr;
    }

    public SExpr getSExpr()
    {
        return _sexpr;
    }
}
