package MScheme.functions;

import MScheme.util.Arity;
import MScheme.machine.Machine;
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
        Machine machine,
        int     len,
        List    args
    ) throws RuntimeError, TypeError;


    // implementation of Function
    
    final public Code call(Machine machine, List arguments)
        throws RuntimeError, TypeError
    {
        return checkedCall(
            machine,
            checkArguments(getArity(), arguments),
            arguments
        );
    }
}
