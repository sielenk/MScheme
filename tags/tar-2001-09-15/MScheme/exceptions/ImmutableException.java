package MScheme.exceptions;

import MScheme.Value;


public class ImmutableException
    extends RuntimeError
{
    public final static String id
        = "$Id$";

    public ImmutableException(Value cause)
    { super(cause); }
}

