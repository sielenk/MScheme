package MScheme.functions;

import MScheme.util.Arity;
import MScheme.machine.State;
import MScheme.code.Code;
import MScheme.values.Value;
import MScheme.values.List;
import MScheme.values.Function;

import MScheme.exceptions.*;


public abstract class CheckedFunction
    extends Function
{
    protected abstract Arity getArity();
    
    protected abstract Code checkedCall(
        State state,
        int   len,
        List  args
    ) throws RuntimeError, TypeError;


    // implementation of Function
    
    final public Code call(State state, List arguments)
        throws RuntimeError, TypeError
    {
        return checkedCall(
            state,
            checkArguments(getArity(), arguments),
            arguments
        );
    }
}
