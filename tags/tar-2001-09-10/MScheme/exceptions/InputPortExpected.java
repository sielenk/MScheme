package MScheme.exceptions;

import MScheme.Value;


public class InputPortExpected
    extends PortExpected
{
    public final static String id
        = "$Id$";

    public InputPortExpected(Value cause)
    { super(cause); }
}

