package MScheme.functions;

import MScheme.machine.Registers;
import MScheme.Value;
import MScheme.Code;
import MScheme.values.ValueFactory;
import MScheme.values.List;

import MScheme.exceptions.*;


public final class CallCCFunction
    extends UnaryFunction
{
    public final static String id
        = "$Id$";

    public final static CallCCFunction INSTANCE = new CallCCFunction();

    private CallCCFunction()
    { }

    protected Code checkedCall(Registers state, Value argument)
        throws RuntimeError, TypeError
    {
        return argument.toFunction().call(
            state,
            ValueFactory.createList(
                state.getCurrentContinuation()
            )
        );
    }
}
