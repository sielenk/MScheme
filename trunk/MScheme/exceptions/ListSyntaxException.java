package MScheme.exceptions;

import MScheme.Value;


public class ListSyntaxException
    extends CompileError
{
    public ListSyntaxException(Value cause)
    { super(cause); }
}

