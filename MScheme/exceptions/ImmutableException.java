package MScheme.exceptions;

import MScheme.Value;


public final class ImmutableException
    extends RuntimeError
{
    public final static String id
        = "$Id$";

    public ImmutableException(Value cause)
    { super(cause, "attempt to modify constant value"); }
}
