package MScheme.exceptions;

import MScheme.values.ScmString;


public class StringException
    extends RuntimeError
{
    public final static String id
        = "$Id$";

    private final int _index;
    
    public StringException(
        ScmString string,
        int       index
    )
    { super(string); _index = index; }
}
