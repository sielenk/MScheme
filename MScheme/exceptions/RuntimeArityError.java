package MScheme.exceptions;

import MScheme.util.Arity;
import MScheme.values.List;


public class RuntimeArityError
    extends RuntimeError
{
    private final Arity _expected;
    
    public RuntimeArityError(List arguments, Arity expected)
    {
        super(arguments.toValue());
        _expected = expected;
    }
}

