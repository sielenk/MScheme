package MScheme.exceptions;

import MScheme.Value;


public class CompileError
    extends SchemeException
{
    public final static String id
        = "$Id$";

    public CompileError(Value cause, String message)
    { super(cause, message); }
}
