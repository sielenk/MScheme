package MScheme.functions;

import MScheme.util.Arity;
import MScheme.machine.Registers;
import MScheme.Code;
import MScheme.Value;
import MScheme.values.List;
import MScheme.values.Function;

import MScheme.exceptions.RuntimeError;
import MScheme.exceptions.TypeError;


public abstract class TernaryFunction
    extends CheckedFunction
{
    public final static String id
        = "$Id$";

    private final static Arity _ternary = Arity.exactly(3);

    public final Arity getArity()
    { return _ternary; }

    protected final Code checkedCall(
        Registers registers,
        int       len,
        List      arguments
    ) throws RuntimeError, TypeError
    {
        return checkedCall(
            registers,
            arguments.getHead(),
            arguments.getTail().getHead(),
            arguments.getTail().getTail().getHead()
        );
    }

    protected abstract Code checkedCall(
        Registers registers,
        Value fst,
        Value snd,
        Value trd
    ) throws RuntimeError, TypeError;
}
