package MScheme.exceptions;

import MScheme.Value;


public class ParseException
    extends RuntimeError
{
    private final String _message;
    
    public ParseException(Value port, String message)
    {
        super(port);
        _message = message;
    }
}

