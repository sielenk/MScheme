package MScheme.functions;

import MScheme.machine.Registers;
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

    protected Code checkedCall(Registers registers, Value argument)
        throws RuntimeError, TypeError
    {
        return argument.toFunction().call(
            registers,
            ValueFactory.createList(
                registers.getCurrentContinuation()
            )
        );
    }
}
