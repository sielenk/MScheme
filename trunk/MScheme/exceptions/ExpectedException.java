package MScheme.exceptions;

import MScheme.values.Value;


public class ExpectedException
    extends SchemeException
{
    public ExpectedException(Value cause)
    {
        super(cause);
    }
}

