package MScheme.exceptions;

import MScheme.values.Value;


public class CharExpected
    extends TypeError
{
    public CharExpected(Value cause)
    { super(cause); }
}
