package MScheme.functions;

import MScheme.util.Arity;
import MScheme.machine.Machine;
import MScheme.code.Code;
import MScheme.values.Value;
import MScheme.values.List;
import MScheme.values.Function;

import MScheme.exceptions.SchemeException;
import MScheme.exceptions.ArityException;


abstract public class BinaryFunction
    extends Function
{
    private final static Arity _binary = Arity.exactly(2);
    
    
    final public Code call(Machine machine, List arguments)
        throws SchemeException
    {
        int len = arguments.getLength();
        
        if (!_binary.isValid(len)) {
            throw new ArityException(arguments, _binary);
        }
        
        return checkedCall(
            machine,
            arguments.getHead(),
            arguments.getTail().getHead()
        );
    }

    abstract protected Code checkedCall(
        Machine machine,
        Value   fst,
        Value   snd
    ) throws SchemeException;
}
