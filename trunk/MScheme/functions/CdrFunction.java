package MScheme.functions;

import MScheme.util.Arity;

import MScheme.machine.Machine;
import MScheme.code.Code;
import MScheme.values.Value;
import MScheme.values.Pair;

import MScheme.exceptions.SchemeException;


public class CdrFunction
    extends UnaryFunction
{
    public final static CdrFunction INSTANCE = new CdrFunction();


    protected Code checkedCall(
        Machine machine,
        Value   argument
    ) throws SchemeException
    {
        return machine.handleResult(
            argument.toPair().getSecond()
        );
    }
}

