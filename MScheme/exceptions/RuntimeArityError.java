package MScheme.exceptions;

import MScheme.util.Arity;
import MScheme.List;


public class RuntimeArityError
    extends RuntimeError
{
    public final static String id
        = "$Id$";

    private final Arity _expected;
    
    public RuntimeArityError(List arguments, Arity expected)
    {
        super(arguments);
        _expected = expected;
    }
    
    public String toString()
    {
        return
	        "expected " +
		    _expected.toString() +
		    '\n' +
	        super.toString();
    }
}

