package MScheme.exceptions;

import MScheme.Value;


public class CharExpected
    extends TypeError
{
    public CharExpected(Value cause)
    { super(cause); }
}
