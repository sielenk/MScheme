package MScheme.functions;

import MScheme.machine.Machine;
import MScheme.code.Code;
import MScheme.values.ValueFactory;
import MScheme.values.Value;
import MScheme.values.Function;

import MScheme.exceptions.SchemeException;


public class DynamicWindFunction
    extends TernaryFunction
{
    public final static DynamicWindFunction INSTANCE
        = new DynamicWindFunction();


    protected Code checkedCall(
        Machine machine,
        Value   fst,
        Value   snd,
        Value   trd
    ) throws SchemeException
    {
        Function before = fst.toFunction();
        Function thunk  = snd.toFunction();
        Function after  = trd.toFunction();

        return machine.handleDynamicWind(before, thunk, after);
    }
}

