package MScheme.functions;

import MScheme.util.Arity;
import MScheme.machine.Registers;
import MScheme.Code;
import MScheme.values.List;
import MScheme.values.Function;

import MScheme.exceptions.RuntimeError;
import MScheme.exceptions.TypeError;


public abstract class Thunk
    extends CheckedFunction
{
    public final static String id
        = "$Id$";

    private final static Arity _none = Arity.exactly(0);

    public final Arity getArity()
    { return _none; }

    protected final Code checkedCall(
        Registers state,
        int       len,
        List      arguments
    ) throws RuntimeError, TypeError
    { return checkedCall(state); }

    protected abstract Code checkedCall(
        Registers state
    ) throws RuntimeError, TypeError;
}
