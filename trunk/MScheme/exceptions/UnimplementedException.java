package MScheme.exceptions;

import MScheme.Value;


public class UnimplementedException
    extends SchemeException
{
    public final static String id
        = "$Id$";

    public UnimplementedException(Value cause)
    { super(cause); }
}

