package MScheme.exceptions;

import MScheme.values.Value;
import MScheme.values.*;


public class ParseException
    extends SchemeException
{
    private final String _message;
    
    public ParseException(Value port, String message)
    {
        super(port);
        _message = message;
    }
}

