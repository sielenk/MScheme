package MScheme.exceptions;

import MScheme.values.ScmVector;


public class InvalidVectorIndexException
            extends VectorException
{
    public final static String id
    = "$Id$";

    public InvalidVectorIndexException(
        ScmVector vector,
        int       index
    )
    {
        super(vector, index);
    }
}

