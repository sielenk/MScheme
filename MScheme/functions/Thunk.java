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
    private final static Arity _none = Arity.exactly(0);

    protected final Arity getArity()
    { return _none; }

    protected final Code checkedCall(
        Registers registers,
        int       len,
        List      arguments
    ) throws RuntimeError, TypeError
    { return checkedCall(registers); }

    protected abstract Code checkedCall(
        Registers registers
    ) throws RuntimeError, TypeError;
}
