package MScheme.exceptions;

import MScheme.values.ScmVector;


public class InvalidVectorIndexException
    extends VectorException
{
    public InvalidVectorIndexException(
        ScmVector vector,
        int       index
    )
    { super(vector, index); }
}

