package MScheme.exceptions;

import MScheme.values.SchemeString;


public class InvalidStringIndexException
    extends StringException
{
    public InvalidStringIndexException(
        SchemeString string,
        int          index
    )
    { super(string, index); }
}
