package MScheme.exceptions;

import MScheme.Value;


public class RuntimeError
            extends SchemeException
{
    public final static String id
    = "$Id$";

    public RuntimeError(Value cause, String message)
    {
        super(cause, message);
    }

    public RuntimeError(Value cause)
    {
        this(cause, "");
    }
}
