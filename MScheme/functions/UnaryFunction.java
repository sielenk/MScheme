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
    public final static String id
        = "$Id$";

    private final static Arity _unary = Arity.exactly(1);

    public final Arity getArity()
    { return _unary; }

    protected final Code checkedCall(
        Registers state,
        int       len,
        List      arguments
    ) throws RuntimeError, TypeError
    {
        return checkedCall(
            state,
            arguments.getHead()
        );
    }

    protected abstract Code checkedCall(
        Registers state,
        Value     fst
    ) throws RuntimeError, TypeError;
}
