package MScheme.exceptions;

import MScheme.values.ScmVector;


public class VectorException
    extends RuntimeError
{
    private final int _index;
    
    public VectorException(
        ScmVector vector,
        int       index
    )
    { super(vector); _index = index; }
}

