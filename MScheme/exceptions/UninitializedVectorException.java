package MScheme.exceptions;

import MScheme.values.SchemeVector;


public class UninitializedVectorException
    extends VectorException
{
    public UninitializedVectorException(
        SchemeVector vector,
        int          index
    )
    { super(vector, index); }
}

