package MScheme.values.functions;

import MScheme.machine.Registers;
import MScheme.Code;
import MScheme.Value;

import MScheme.exceptions.SchemeException;


public abstract class ValueThunk
            extends Thunk
{
    public final static String id
    = "$Id$";


    protected final Code checkedCall(
        Registers state
    ) throws SchemeException
    {
        return checkedCall().getLiteral();
    }

    protected abstract Value checkedCall()
    throws SchemeException;
}
