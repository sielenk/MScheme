package MScheme.exceptions;

import MScheme.values.Value;


public class PairExpected
    extends ListExpected
{
    public PairExpected(Value cause)
    {
        super(cause);
    }
}
