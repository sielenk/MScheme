package MScheme.exceptions;

import MScheme.values.Value;


public class PairExpectedException
    extends ListExpectedException
{
    public PairExpectedException(Value cause)
    {
        super(cause);
    }
}

