package MScheme.exceptions;

import MScheme.values.SchemeString;


public class StringException
    extends RuntimeError
{
    private final int _index;
    
    public StringException(
        SchemeString string,
        int          index
    )
    { super(string); _index = index; }
}
