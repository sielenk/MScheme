package MScheme.functions;

import MScheme.machine.Machine;
import MScheme.values.Value;
import MScheme.code.Code;
import MScheme.values.ValueFactory;
import MScheme.values.Value;
import MScheme.values.List;

import MScheme.exceptions.*;


public class CallCCFunction
    extends UnaryFunction
{
    public final static CallCCFunction INSTANCE = new CallCCFunction();
    
    
    protected Code checkedCall(Machine machine, Value argument)
        throws RuntimeError, TypeError
    {
        return argument.toFunction().call(
            machine,
            ValueFactory.createList(
                machine.getContinuation()
            )
        );
    }
}

