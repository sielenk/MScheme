package MScheme.functions;

import MScheme.util.Arity;
import MScheme.machine.Registers;
import MScheme.Code;
import MScheme.Value;
import MScheme.values.List;
import MScheme.values.Function;

import MScheme.exceptions.RuntimeError;
import MScheme.exceptions.TypeError;


public abstract class BinaryFunction
    extends CheckedFunction
{
    private final static Arity _binary = Arity.exactly(2);

    protected final Arity getArity()
    { return _binary; }

    protected final Code checkedCall(
        Registers registers,
        int       len,
        List      arguments
    ) throws RuntimeError, TypeError
    {
        return checkedCall(
            registers,
            arguments.getHead(),
            arguments.getTail().getHead()
        );
    }

    protected abstract Code checkedCall(
        Registers registers,
        Value fst,
        Value snd
    ) throws RuntimeError, TypeError;
}
