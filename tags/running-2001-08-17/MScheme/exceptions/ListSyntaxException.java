package MScheme.exceptions;

import MScheme.values.Value;


public class ListSyntaxException
    extends CompileError
{
    public ListSyntaxException(Value cause)
    { super(cause); }
}

