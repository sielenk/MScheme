package MScheme.values.functions;

import MScheme.Value;
import MScheme.Code;

import MScheme.machine.Registers;

import MScheme.exceptions.SchemeException;


public abstract class UnaryValueFunction
            extends UnaryFunction
{
    public final static String id
    = "$Id$";


    protected final Code checkedCall(
        Registers state,
        Value     fst
    ) throws SchemeException
    {
        return checkedCall(fst).getLiteral();
    }

    protected abstract Value checkedCall(Value fst)
    throws SchemeException;
}
