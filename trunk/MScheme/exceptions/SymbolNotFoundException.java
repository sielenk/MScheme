package MScheme.exceptions;

import MScheme.Value;


public final class SymbolNotFoundException
    extends CompileError
{
    public final static String id
        = "$Id$";

    public SymbolNotFoundException(Value cause)
    { super(cause, "symbol not found"); }
}
