package MScheme.functions;

import MScheme.machine.Registers;

import MScheme.Value;
import MScheme.Code;

import MScheme.values.ListFactory;
import MScheme.values.List;

import MScheme.exceptions.*;


public final class SpawnFunction
    extends UnaryFunction
{
    public final static String id
        = "$Id$";


    public final static SpawnFunction INSTANCE = new SpawnFunction();

    private SpawnFunction()
    { }

    protected Code checkedCall(Registers state, Value argument)
        throws SchemeException
    {
        return argument.toFunction().call(
            state,
            ListFactory.create(
                state.getComputationController()
            )
        );
    }
}
