package MScheme.exceptions;

import MScheme.Value;


public final class DuplicateSymbolException
            extends CompileError
{
    public final static String id
    = "$Id$";

    public DuplicateSymbolException(Value cause)
    {
        super(cause, "duplicate symbol");
    }
}
