package MScheme.functions;

import MScheme.machine.Registers;
import MScheme.Code;
import MScheme.Value;

import MScheme.exceptions.SchemeException;


public abstract class TernaryValueFunction
            extends TernaryFunction
{
    public final static String id
    = "$Id$";


    protected final Code checkedCall(
        Registers state,
        Value     fst,
        Value     snd,
        Value     trd
    ) throws SchemeException
    {
        return checkedCall(fst, snd, trd).getLiteral();
    }

    protected abstract Value checkedCall(Value fst, Value snd, Value trd)
    throws SchemeException;
}
