package MScheme.exceptions;

import MScheme.values.ScmVector;


public class VectorException
    extends RuntimeError
{
    public final static String id
        = "$Id$";

    private final int _index;
    
    public VectorException(
        ScmVector vector,
        int       index
    )
    { super(vector); _index = index; }
}

