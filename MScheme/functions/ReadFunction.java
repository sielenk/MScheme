package MScheme.functions;

import MScheme.machine.Machine;
import MScheme.code.Code;
import MScheme.values.*;

import MScheme.exceptions.SchemeException;


public class ReadFunction
    extends UnaryFunction
{
    public final static ReadFunction INSTANCE = new ReadFunction();
    
    protected Code checkedCall(Machine machine, Value argument)
        throws SchemeException
    {
        return machine.handleResult(
            argument.toPort().toInput().read()
        );
    }
}

