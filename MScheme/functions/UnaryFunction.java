package MScheme.functions;

import MScheme.util.Arity;
import MScheme.machine.Registers;
import MScheme.Code;
import MScheme.Value;
import MScheme.values.List;
import MScheme.values.Function;

import MScheme.exceptions.SchemeException;


public abstract class UnaryFunction
            extends CheckedFunction
{
    public final static String id
    = "$Id$";


    protected final Arity getArity()
    {
        return Arity.exactly(1);
    }

    protected final Code checkedCall(
        Registers state,
        int       len,
        List      arguments
    ) throws SchemeException
    {
        return checkedCall(
                   state,
                   arguments.getHead()
               );
    }

    protected abstract Code checkedCall(
        Registers state,
        Value     fst
    ) throws SchemeException;
}
