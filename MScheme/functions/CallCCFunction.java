package MScheme.functions;

import MScheme.machine.Registers;

import MScheme.Value;
import MScheme.List;
import MScheme.Code;

import MScheme.values.ListFactory;

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
    throws SchemeException
    {
        return argument.toFunction().call(
                   state,
                   ListFactory.create(
                       state.getCurrentContinuation()
                   )
               );
    }
}
