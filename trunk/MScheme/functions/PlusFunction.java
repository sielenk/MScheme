package MScheme.functions;

import MScheme.machine.Machine;
import MScheme.code.Code;
import MScheme.values.Value;

import MScheme.exceptions.SchemeException;


public class PlusFunction
    extends BinaryFunction
{
    public final static PlusFunction INSTANCE = new PlusFunction();


    protected Code checkedCall(
        Machine machine,
        Value   fst,
        Value   snd
    ) throws SchemeException
    {
        return machine.handleResult(
            fst.toNumber().plus(snd.toNumber())
        );
    }
}

