package MScheme.exceptions;

import MScheme.values.Value;


public class ListExpectedException
    extends ExpectedException
{
    public ListExpectedException(Value cause)
    { super(cause); }
}

