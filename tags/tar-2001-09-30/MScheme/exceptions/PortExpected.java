package MScheme.exceptions;

import MScheme.Value;


public abstract class PortExpected
    extends TypeError
{
    public final static String id
        = "$Id$";

    public PortExpected(Value cause, String message)
    { super(cause, message); }
}
