package MScheme.exceptions;

import MScheme.values.Symbol;


public final class UnexpectedSyntax
    extends CompileError
{
    public final static String id
        = "$Id$";

    public UnexpectedSyntax(Symbol cause)
    { super(cause, "unexpected syntactic keyword"); }
}
