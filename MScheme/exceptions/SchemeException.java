package MScheme.exceptions;

import MScheme.Value;

import MScheme.values.ScmString;


public abstract class SchemeException
            extends Exception
{
    public final static String id
    = "$Id$";

    private final Value _cause;
    private final Value _message;

    public SchemeException(
        Value  cause,
        String message
    )
    {
        _cause   = cause;
        _message = ScmString.createConst(message);
    }

    public Value getCauseValue()
    {
        return _cause;
    }

    public Value getMessageValue()
    {
        return _message;
    }

    public String toString()
    {
        return
            "'" +
            getCauseValue().toString() +
            "' caused a\n" +
            super.toString();
    }
}
