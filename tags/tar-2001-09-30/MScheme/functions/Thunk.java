package MScheme.functions;

import MScheme.util.Arity;
import MScheme.machine.Registers;
import MScheme.Code;
import MScheme.List;
import MScheme.values.Function;

import MScheme.exceptions.SchemeException;


public abstract class Thunk
    extends CheckedFunction
{
    public final static String id
        = "$Id$";


    protected final Arity getArity()
    { return Arity.exactly(0); }

    protected final Code checkedCall(
        Registers state,
        int       len,
        List      arguments
    ) throws SchemeException
    { return checkedCall(state); }

    protected abstract Code checkedCall(
        Registers state
    ) throws SchemeException;
}
