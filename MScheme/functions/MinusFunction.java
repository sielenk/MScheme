package MScheme.functions;

import MScheme.util.Arity;

import MScheme.machine.Machine;
import MScheme.code.Code;
import MScheme.values.Value;

import MScheme.exceptions.TypeError;


public class MinusFunction
    extends CheckedFunction
{
    public final static MinusFunction INSTANCE = new MinusFunction();

    private MinusFunction()
    { super(Arity.inRange(1, 2)); }


    protected Code checkedCall(
        Machine machine,
        Value   value
    ) throws TypeError
    {
        return machine.handleResult(
            value.toNumber().negated()
        );
    }

    protected Code checkedCall(
        Machine machine,
        Value   fst,
        Value   snd
    ) throws TypeError
    {
        return machine.handleResult(
            fst.toNumber().minus(snd.toNumber())
        );
    }
}

