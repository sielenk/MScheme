package MScheme.functions;

import MScheme.util.Arity;
import MScheme.machine.State;
import MScheme.code.Code;
import MScheme.values.Value;
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
        State state,
        int   len,
        List  arguments
    ) throws RuntimeError, TypeError
    {
        return checkedCall(
            state,
            arguments.getHead(),
            arguments.getTail().getHead()
        );
    }

    protected abstract Code checkedCall(
        State state,
        Value fst,
        Value snd
    ) throws RuntimeError, TypeError;
}
