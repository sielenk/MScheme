package MScheme.functions;

import MScheme.util.Arity;

import MScheme.machine.Registers;
import MScheme.Code;
import MScheme.Value;
import MScheme.values.Function;
import MScheme.values.List;
import MScheme.values.Pair;

import MScheme.exceptions.SchemeException;


public class ApplyFunction
            extends CheckedFunction
{
    public final static String id
    = "$Id$";


    public final static ApplyFunction INSTANCE = new ApplyFunction();


    protected Arity getArity()
    {
        return Arity.atLeast(2);
    }

    protected Code checkedCall(
        Registers state,
        int       length,
        List      arguments
    ) throws SchemeException
    {
        // First try the cast ... if it fails, no work is lost
        Function func = arguments.getHead().toFunction();

        // Since the argument list is newly allocated
        // and mutable, it is permissible to modify it. (*)
        // The modification done looks like this:
        // (f 0 1 (2 3)) is changed to (f 0 1 2 3)

        List toBeModified = arguments;
        for (int i = length - 2; i > 0; i--)
        {
            toBeModified = toBeModified.getTail();
        }

        // Now toBeModified referes the pair containing the
        // last but one argument. In the example it would be
        // (1 . ((2 3)))

        toBeModified.toPair().setSecond(
            toBeModified.getTail().getHead().toList().getCopy()
        );

        // and here it would have become
        // (1 . (2 3)) == (1 2 3)
        
        // the call to getCopy() is necessary, because of the
        // statement marked with (*) above.

        return func.call(state, arguments.getTail());
    }
}
