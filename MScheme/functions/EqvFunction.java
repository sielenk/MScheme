package MScheme.functions;

import MScheme.machine.Machine;
import MScheme.code.Code;
import MScheme.values.ValueFactory;
import MScheme.values.Value;

import MScheme.exceptions.SchemeException;


public class EqvFunction
    extends BinaryFunction
{
    public final static EqvFunction INSTANCE = new EqvFunction();
    
    
    protected Code checkedCall(
        Machine machine,
        Value   fst,
        Value   snd
    ) throws SchemeException
    {
        return machine.handleResult(
            ValueFactory.createBool(fst.eqv(snd))
        );
    }
}

