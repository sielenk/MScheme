package MScheme.functions;

import MScheme.util.Arity;

import MScheme.machine.Registers;
import MScheme.Code;
import MScheme.Value;
import MScheme.values.Function;
import MScheme.values.List;
import MScheme.values.Pair;

import MScheme.exceptions.*;


public class ApplyFunction
    extends CheckedFunction
{
    public final static String id
        = "$Id$";


    public final static ApplyFunction INSTANCE = new ApplyFunction();


    protected Arity getArity()
    { return Arity.atLeast(2); }

    protected Code checkedCall(
        Registers state,
        int       length,
        List      arguments
    ) throws RuntimeError, TypeError
    {
        // First try the cast ... if it fails, no work is lost
        Function func = arguments.getHead().toFunction();

        // Since the argument list is newly allocated
        // and mutable, it is permissible to modify it.
        // The modification done looks like this:
        // (f 1 (2 3)) is changed to (f 1 2 3)

        Pair toBeModified = arguments.toPair();
        for (int i = length - 2; i > 0; i--) {
            toBeModified = toBeModified.getSecond().toPair();
        }

        // Now toBeModified referes the pair containing the
        // last but one argument. In the example it would be
        // (1 . ((2 3) . ()))

        toBeModified.setSecond(
            toBeModified.getSecond().toPair().getFirst()
        );

        // and here it would have become
        // (1 . (2 3)) == (1 2 3)

        return func.call(state, arguments.getTail());
    }
}
