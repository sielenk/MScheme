package MScheme.functions;

import MScheme.util.Arity;
import MScheme.machine.Registers;
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
        Registers registers,
        int       len,
        List      args
    ) throws RuntimeError, TypeError;


    // implementation of Function
    
    final public Code call(Registers registers, List arguments)
        throws RuntimeError, TypeError
    {
        return checkedCall(
            registers,
            checkArguments(getArity(), arguments),
            arguments
        );
    }
}