package MScheme.exceptions;

import MScheme.expressions.SExpr;

public class SWrongArgumentCountException extends SException
{
    private int _expectedMin;
    private int _expectedMax;

    public SWrongArgumentCountException(
        SExpr sexpr,
        int   expectedMin,
        int   expectedMax
    ) {
        super(sexpr);

        _expectedMin = expectedMin;
        _expectedMax = expectedMax;
    }

    public int getExpectedMin()
    {
        return _expectedMin;
    }

    public int getExpectedMax()
    {
        return _expectedMax;
    }
}
