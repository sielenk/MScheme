package MScheme.exceptions;

import MScheme.values.Value;


public class SchemeException
    extends Exception
{
    private final Value _cause;
    
    public SchemeException(Value cause)
    { _cause = cause; }
    
    public Value getCause()
    { return _cause; }
}

