package MScheme.functions;

import MScheme.util.Arity;

import MScheme.machine.Registers;

import MScheme.Code;
import MScheme.Value;
import MScheme.List;

import MScheme.values.ListFactory;
import MScheme.values.Function;

import MScheme.exceptions.*;


class YWrappedFunction
            extends Function
{
    public final static String id
    = "$Id$";


    private final Function _f;

    YWrappedFunction(Function f)
    {
        _f = f;
    }

    public Code call(Registers state, List arguments)
    throws SchemeException
    {
        return _f.call(
                   state,
                   ListFactory.prepend(
                       this,
                       arguments
                   )
               );
    }
}

public final class YCombinator
            extends UnaryValueFunction
{
    public final static String id
    = "$Id$";


    public final static YCombinator INSTANCE = new YCombinator();

    protected Value checkedCall(Value fst)
    throws TypeError
    {
        return new YWrappedFunction(fst.toFunction());
    }
}
