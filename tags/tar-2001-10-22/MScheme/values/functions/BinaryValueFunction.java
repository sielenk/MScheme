package MScheme.values.functions;

import MScheme.machine.Registers;
import MScheme.Code;
import MScheme.Value;

import MScheme.exceptions.SchemeException;


public abstract class BinaryValueFunction
            extends BinaryFunction
{
    public final static String id
    = "$Id$";


    protected final Code checkedCall(
        Registers state,
        Value     fst,
        Value     snd
    ) throws SchemeException
    {
        return checkedCall(fst, snd).getLiteral();
    }

    protected abstract Value checkedCall(Value fst, Value snd)
    throws SchemeException;
}
