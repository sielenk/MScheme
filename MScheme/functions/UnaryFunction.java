package MScheme.functions;

import MScheme.util.Arity;
import MScheme.machine.Registers;
import MScheme.Code;
import MScheme.Value;
import MScheme.values.List;
import MScheme.values.Function;

import MScheme.exceptions.RuntimeError;
import MScheme.exceptions.TypeError;


public abstract class UnaryFunction
    extends CheckedFunction
{
    private final static Arity _unary = Arity.exactly(1);

    public final Arity getArity()
    { return _unary; }

    protected final Code checkedCall(
        Registers registers,
        int       len,
        List      arguments
    ) throws RuntimeError, TypeError
    {
        return checkedCall(
            registers,
            arguments.getHead()
        );
    }

    protected abstract Code checkedCall(
        Registers registers,
        Value     fst
    ) throws RuntimeError, TypeError;
}
