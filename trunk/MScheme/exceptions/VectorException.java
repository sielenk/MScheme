package MScheme.exceptions;

import MScheme.values.SchemeVector;


public class VectorException
    extends RuntimeError
{
    private final int _index;
    
    public VectorException(
        SchemeVector vector,
        int          index
    )
    { super(vector); _index = index; }
}

