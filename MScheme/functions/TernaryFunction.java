package MScheme.functions;

import MScheme.util.Arity;
import MScheme.machine.Registers;
import MScheme.Code;
import MScheme.Value;
import MScheme.List;
import MScheme.values.Function;

import MScheme.exceptions.SchemeException;


public abstract class TernaryFunction
            extends CheckedFunction
{
    public final static String id
    = "$Id$";


    protected final Arity getArity()
    {
        return Arity.exactly(3);
    }

    protected final Code checkedCall(
        Registers state,
        int       len,
        List      arguments
    ) throws SchemeException
    {
        return checkedCall(
                   state,
                   arguments.getHead(),
                   arguments.getTail().getHead(),
                   arguments.getTail().getTail().getHead()
               );
    }

    protected abstract Code checkedCall(
        Registers state,
        Value fst,
        Value snd,
        Value trd
    ) throws SchemeException;
}
