package MScheme.functions;

import MScheme.util.Arity;
import MScheme.machine.State;
import MScheme.code.Code;
import MScheme.values.List;
import MScheme.values.Function;

import MScheme.exceptions.RuntimeError;
import MScheme.exceptions.TypeError;


public abstract class Thunk
    extends CheckedFunction
{
    private final static Arity _none = Arity.exactly(0);

    protected final Arity getArity()
    { return _none; }

    protected final Code checkedCall(
        State state,
        int   len,
        List  arguments
    ) throws RuntimeError, TypeError
    { return checkedCall(state); }

    protected abstract Code checkedCall(
        State state
    ) throws RuntimeError, TypeError;
}
