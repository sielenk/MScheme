package MScheme.exceptions;

import MScheme.expressions.SExpr;

public class SExpectedListException extends SException
{
    public SExpectedListException(SExpr sexpr)
    {
        super(sexpr);
    }
}
