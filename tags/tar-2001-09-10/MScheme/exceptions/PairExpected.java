package MScheme.exceptions;

import MScheme.Value;


public class PairExpected
    extends ListExpected
{
    public final static String id
        = "$Id$";

    public PairExpected(Value cause)
    {
        super(cause);
    }
}
