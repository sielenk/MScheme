package MScheme.exceptions;

import MScheme.Value;


public class SymbolExpected
    extends TypeError
{
    public final static String id
        = "$Id$";

    public SymbolExpected(Value cause)
    { super(cause); }
}

