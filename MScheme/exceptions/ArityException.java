package MScheme.exceptions;

import MScheme.util.Arity;
import MScheme.values.Value;
import MScheme.values.*;


public class ArityException
    extends SchemeException
{
    private final Arity _expected;
    
    public ArityException(List arguments, Arity expected)
    {
        super(arguments);
        _expected = expected;
    }
}

