package MScheme.exceptions;

import MScheme.util.Arity;
import MScheme.values.List;


public class SyntaxArityError
    extends CompileError
{
    public final static String id
        = "$Id$";

    private final Arity _expected;
    
    public SyntaxArityError(List arguments, Arity expected)
    {
        super(arguments);
        _expected = expected;
    }
}
