package MScheme.functions;

import MScheme.util.Arity;
import MScheme.machine.Machine;
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
        Machine machine,
        int     len,
        List    arguments
    ) throws RuntimeError, TypeError
    {
        return checkedCall(
            machine
        );
    }

    protected abstract Code checkedCall(
        Machine machine
    ) throws RuntimeError, TypeError;
}
