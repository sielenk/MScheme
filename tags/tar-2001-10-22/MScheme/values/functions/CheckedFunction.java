package MScheme.values.functions;

import MScheme.util.Arity;

import MScheme.machine.Registers;

import MScheme.Code;
import MScheme.Value;

import MScheme.values.List;
import MScheme.values.Function;

import MScheme.exceptions.SchemeException;


public abstract class CheckedFunction
    extends Function
{
    public final static String id
        = "$Id$";


    protected abstract Arity getArity();

    protected abstract Code checkedCall(
        Registers state,
        int       len,
        List      args
    ) throws SchemeException;


    // implementation of Function

    public final Code call(Registers state, List arguments)
        throws SchemeException
    {
        return checkedCall(
            state,
            checkArguments(getArity(), arguments),
            arguments
        );
    }
}
