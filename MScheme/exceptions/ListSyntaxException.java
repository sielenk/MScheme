package MScheme.exceptions;

import MScheme.Value;


public class ListSyntaxException
    extends CompileError
{
    public final static String id
        = "$Id$";

    public ListSyntaxException(Value cause)
    { super(cause); }
}

