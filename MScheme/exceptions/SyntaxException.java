package MScheme.exceptions;

import MScheme.values.Value;


public class SyntaxException
    extends SchemeException
{
    public SyntaxException(Value cause)
    { super(cause); }
}

