package MScheme.functions;

import MScheme.machine.Machine;
import MScheme.code.Code;
import MScheme.values.ValueFactory;
import MScheme.values.Value;

import MScheme.exceptions.SchemeException;


public class EqualFunction
    extends BinaryFunction
{
    public final static EqualFunction INSTANCE = new EqualFunction();
    
    
    protected Code checkedCall(
        Machine machine,
        Value   fst,
        Value   snd
    ) throws SchemeException
    {
        return machine.handleResult(
            ValueFactory.createBool(fst.equal(snd))
        );
    }
}

