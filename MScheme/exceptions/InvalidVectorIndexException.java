package MScheme.exceptions;

import MScheme.values.SchemeVector;


public class InvalidVectorIndexException
    extends VectorException
{
    public InvalidVectorIndexException(
        SchemeVector vector,
        int index
    )
    { super(vector, index); }
}

