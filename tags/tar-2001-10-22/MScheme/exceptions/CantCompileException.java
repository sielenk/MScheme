package MScheme.exceptions;

import MScheme.Value;


public final class CantCompileException
            extends CompileError
{
    public final static String id
    = "$Id$";

    public CantCompileException(Value cause)
    {
        super(cause, "can't compile");
    }
}
