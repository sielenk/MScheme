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
    public final static String id
        = "$Id$";

    private final static Arity _binary = Arity.exactly(2);

    public final Arity getArity()
    { return _binary; }

    protected final Code checkedCall(
        Registers state,
        int       len,
        List      arguments
    ) throws RuntimeError, TypeError
    {
        return checkedCall(
            state,
            arguments.getHead(),
            arguments.getTail().getHead()
        );
    }

    protected abstract Code checkedCall(
        Registers state,
        Value fst,
        Value snd
    ) throws RuntimeError, TypeError;
}
