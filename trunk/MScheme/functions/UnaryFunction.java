package MScheme.functions;

import MScheme.util.Arity;
import MScheme.machine.State;
import MScheme.code.Code;
import MScheme.values.Value;
import MScheme.values.List;
import MScheme.values.Function;

import MScheme.exceptions.RuntimeError;
import MScheme.exceptions.TypeError;


public abstract class UnaryFunction
    extends CheckedFunction
{
    private final static Arity _unary = Arity.exactly(1);

    protected final Arity getArity()
    { return _unary; }

    protected final Code checkedCall(
        State state,
        int   len,
        List  arguments
    ) throws RuntimeError, TypeError
    {
        return checkedCall(
            state,
            arguments.getHead()
        );
    }

    protected abstract Code checkedCall(
        State state,
        Value fst
    ) throws RuntimeError, TypeError;
}
