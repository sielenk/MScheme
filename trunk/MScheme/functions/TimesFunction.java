package MScheme.functions;

import MScheme.machine.Machine;
import MScheme.values.Value;
import MScheme.code.Code;
import MScheme.values.ValueFactory;
import MScheme.values.Value;
import MScheme.values.List;

import MScheme.exceptions.SchemeException;


public class TimesFunction
    extends BinaryFunction
{
    public final static TimesFunction INSTANCE = new TimesFunction();


    protected Code checkedCall(
        Machine machine,
        Value   fst,
        Value   snd
    ) throws SchemeException
    {
        return machine.handleResult(
            fst.toNumber().times(snd.toNumber())
        );
    }
}

