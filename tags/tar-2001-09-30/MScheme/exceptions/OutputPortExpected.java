package MScheme.exceptions;

import MScheme.Value;


public final class OutputPortExpected
    extends PortExpected
{
    public final static String id
        = "$Id$";

    public OutputPortExpected(Value cause)
    { super(cause, "output port"); }
}
