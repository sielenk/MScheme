package MScheme.exceptions;

import MScheme.Value;


public class VectorExpected
    extends TypeError
{
    public VectorExpected(Value cause)
    { super(cause); }
}
