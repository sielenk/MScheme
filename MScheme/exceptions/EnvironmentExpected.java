package MScheme.exceptions;

import MScheme.Value;


public class EnvironmentExpected
    extends TypeError
{
    public final static String id
        = "$Id$";

    public EnvironmentExpected(Value cause)
    { super(cause); }
}
