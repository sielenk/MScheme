package MScheme.functions;

import MScheme.util.Arity;

import MScheme.machine.Machine;
import MScheme.code.Code;
import MScheme.values.Value;
import MScheme.values.Function;
import MScheme.values.List;
import MScheme.values.Pair;

import MScheme.exceptions.*;


public class ApplyFunction
    extends CheckedFunction
{
    public final static ApplyFunction INSTANCE = new ApplyFunction();

    private ApplyFunction()
    { super(Arity.atLeast(2), false); }
    

    protected Code checkedCall(
        Machine machine,
        List    arguments
    ) throws RuntimeError, TypeError
    {
        {
            Pair toBeModified = arguments.toPair();
            for (int i = arguments.getLength() - 2; i > 0; i--) {
                toBeModified = toBeModified.getSecond().toPair();
            }
            toBeModified.setSecond(
                toBeModified.getSecond().toPair().getFirst()
            );
        }

        {
            Function func  = arguments.getHead().toFunction();
            List     args  = arguments.getTail();
            return func.call(machine, args);
        }
    }
}

