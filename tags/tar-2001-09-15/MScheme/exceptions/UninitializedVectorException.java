package MScheme.exceptions;

import MScheme.values.ScmVector;


public class UninitializedVectorException
    extends VectorException
{
    public final static String id
        = "$Id$";

    public UninitializedVectorException(
        ScmVector vector,
        int       index
    )
    { super(vector, index); }
}

