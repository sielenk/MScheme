package MScheme.exceptions;

import MScheme.util.Arity;
import MScheme.List;


public final class SyntaxArityError
            extends CompileError
{
    public final static String id
    = "$Id$";

    public SyntaxArityError(List arguments, Arity expected)
    {
        super(arguments, "expected " + expected.toString() + " arguments");
    }
}
