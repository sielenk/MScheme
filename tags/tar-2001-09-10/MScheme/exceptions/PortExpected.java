package MScheme.exceptions;

import MScheme.Value;


public class PortExpected
    extends TypeError
{
    public final static String id
        = "$Id$";

    public PortExpected(Value cause)
    { super(cause); }
}
