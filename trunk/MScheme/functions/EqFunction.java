package MScheme.functions;

import MScheme.machine.Machine;
import MScheme.code.Code;
import MScheme.values.ValueFactory;
import MScheme.values.Value;

import MScheme.exceptions.SchemeException;


public class EqFunction
    extends BinaryFunction
{
    public final static EqFunction INSTANCE = new EqFunction();
    
    
    protected Code checkedCall(
        Machine machine,
        Value   fst,
        Value   snd
    ) throws SchemeException
    {
        return machine.handleResult(
            ValueFactory.createBool(fst.eq(snd))
        );
    }
}

