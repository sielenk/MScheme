package MScheme.exceptions;

import MScheme.util.Arity;
import MScheme.values.List;


public class SyntaxArityError
    extends CompileError
{
    private final Arity _expected;
    
    public SyntaxArityError(List arguments, Arity expected)
    {
        super(arguments.toValue());
        _expected = expected;
    }
}

