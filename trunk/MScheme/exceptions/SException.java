package MScheme.exceptions;

import MScheme.expressions.SExpr;

public abstract class SException extends Exception
{
    private SExpr _sexpr;

    SException(
        SExpr sexpr
    ) {
        super();
        _sexpr = sexpr;
    }

    SExpr getSExpr()
    {
        return _sexpr;
    }
}

