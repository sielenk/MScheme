package MScheme.functions;

import MScheme.util.Arity;
import MScheme.machine.Machine;
import MScheme.code.Code;
import MScheme.values.Value;
import MScheme.values.List;
import MScheme.values.Function;

import MScheme.exceptions.SchemeException;
import MScheme.exceptions.ArityException;


abstract public class UnaryFunction
    extends Function
{
    private final static Arity _unary = Arity.exactly(1);
    
    
    final public Code call(Machine machine, List arguments)
        throws SchemeException
    {
        int len = arguments.getLength();
        
        if (!_unary.isValid(len)) {
            throw new ArityException(arguments, _unary);
        }
        
        return checkedCall(
            machine,
            arguments.getHead()
        );
    }

    abstract protected Code checkedCall(
        Machine machine,
        Value   fst
    ) throws SchemeException;
}
