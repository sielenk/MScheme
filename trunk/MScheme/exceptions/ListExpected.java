package MScheme.exceptions;

import MScheme.Value;


public class ListExpected
    extends TypeError
{
    public ListExpected(Value cause)
    { super(cause); }
}

