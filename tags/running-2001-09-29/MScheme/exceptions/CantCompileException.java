package MScheme.exceptions;

import MScheme.Value;


public class CantCompileException
    extends CompileError
{
    public final static String id
        = "$Id$";

    public CantCompileException(Value cause)
    { super(cause); }
}
