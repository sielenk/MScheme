package MScheme.functions;

import MScheme.util.Arity;
import MScheme.machine.Registers;
import MScheme.Code;
import MScheme.Value;
import MScheme.values.List;
import MScheme.values.Function;

import MScheme.exceptions.*;


public abstract class CheckedFunction
    extends Function
{
    public final static String id
        = "$Id$";

    protected abstract Code checkedCall(
        Registers state,
        int       len,
        List      args
    ) throws RuntimeError, TypeError;


    // partial implementation of Function

    public final Code call(Registers state, List arguments)
        throws RuntimeError, TypeError
    {
        return checkedCall(
            state,
            checkArguments(getArity(), arguments),
            arguments
        );
    }
}
