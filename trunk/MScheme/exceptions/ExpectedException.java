package MScheme.exceptions;

import MScheme.values.Value;


public class ExpectedException
    extends TypeError
{
    public ExpectedException(Value cause)
    {
        super(cause);
    }
}

