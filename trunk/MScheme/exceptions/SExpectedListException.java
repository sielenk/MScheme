package MScheme.exceptions;

import MScheme.expressions.SExpr;

public class SExpectedListException extends SException
{
    SExpectedListException(SExpr sexpr)
    {
        super(sexpr);
    }
}
