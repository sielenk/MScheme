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


    private final static Arity _arity = Arity.atLeast(2);
    
    protected Arity getArity()
    { return _arity; }

    protected Code checkedCall(
        Machine machine,
        int     length,
        List    arguments
    ) throws RuntimeError, TypeError
    {
        Function func  = arguments.getHead().toFunction();

        {
            Pair toBeModified = arguments.toPair();
            for (int i = length - 2; i > 0; i--) {
                toBeModified = toBeModified.getSecond().toPair();
            }
            toBeModified.setSecond(
                toBeModified.getSecond().toPair().getFirst()
            );
        }

        List     args  = arguments.getTail();
        return func.call(machine, args);
    }
}
