package MScheme.exceptions;

import MScheme.util.Arity;
import MScheme.values.List;


public final class RuntimeArityError
            extends RuntimeError
{
    public final static String id
        = "$Id$";

    public RuntimeArityError(List arguments, Arity expected)
    {
        super(
            arguments,
            "expected " + expected.toString() + " argument(s)"
        );
    }
}
