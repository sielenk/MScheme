package MScheme.exceptions;

import MScheme.values.Symbol;


public class AlreadyBound
    extends CompileError
{
    public final static String id
        = "$Id$";

    public AlreadyBound(Symbol cause)
    { super(cause); }
}

