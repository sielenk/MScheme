package MScheme.exceptions;

import MScheme.values.Value;


public class ListExpected
    extends TypeError
{
    public ListExpected(Value cause)
    { super(cause); }
}

