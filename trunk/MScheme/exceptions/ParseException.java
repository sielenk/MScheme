package MScheme.exceptions;

import MScheme.Value;


public class ParseException
            extends RuntimeError
{
    public final static String id
    = "$Id$";

    private final String _message;

    public ParseException(Value port, String message)
    {
        super(port);
        _message = message;
    }
}

