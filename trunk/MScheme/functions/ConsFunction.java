package MScheme.functions;

import MScheme.util.Arity;

import MScheme.machine.Machine;
import MScheme.code.Code;
import MScheme.values.Value;
import MScheme.values.ValueFactory;

import MScheme.exceptions.SchemeException;


public class ConsFunction
    extends BinaryFunction
{
    public final static ConsFunction INSTANCE = new ConsFunction();


    protected Code checkedCall(
        Machine machine,
        Value   fst,
        Value   snd
    ) throws SchemeException
    {
        return machine.handleResult(
            ValueFactory.createPair(fst, snd)
        );
    }
}

