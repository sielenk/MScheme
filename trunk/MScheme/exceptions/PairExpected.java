package MScheme.exceptions;

import MScheme.Value;


public class PairExpected
    extends ListExpected
{
    public PairExpected(Value cause)
    {
        super(cause);
    }
}
