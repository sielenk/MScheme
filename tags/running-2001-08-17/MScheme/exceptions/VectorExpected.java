package MScheme.exceptions;

import MScheme.values.Value;


public class VectorExpected
    extends TypeError
{
    public VectorExpected(Value cause)
    { super(cause); }
}
