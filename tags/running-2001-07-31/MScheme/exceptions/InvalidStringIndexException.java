package MScheme.exceptions;

import MScheme.values.ScmString;


public class InvalidStringIndexException
    extends StringException
{
    public InvalidStringIndexException(
        ScmString string,
        int       index
    )
    { super(string, index); }
}
