package MScheme.exceptions;

import MScheme.Value;


public class DuplicateSymbolException
    extends CompileError
{
    public final static String id
        = "$Id$";

    public DuplicateSymbolException(Value cause)
    {
        super(cause);
    }
}

