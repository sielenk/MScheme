package MScheme.values.functions;

import MScheme.util.Arity;

import MScheme.machine.Registers;

import MScheme.Code;
import MScheme.Value;

import MScheme.values.ListFactory;
import MScheme.values.List;
import MScheme.values.Function;

import MScheme.exceptions.*;


final class AppendHelper1
            extends Reducer
{
    public final static String id
    = "$Id$";


    AppendHelper1(Value initial)
    {
        super(initial);
    }

    protected Value combine(Value fst, Value snd)
    {
        return ListFactory.createPair(fst, snd);
    }
}


final class AppendHelper2
            extends Reducer
{
    public final static String id
    = "$Id$";


    final static AppendHelper2 INSTANCE
    = new AppendHelper2();

    private AppendHelper2()
    {
        super(ListFactory.create());
    }

    protected Value combine(Value fst, Value snd)
    throws RuntimeError, TypeError
    {
        return new AppendHelper1(snd).foldRight(fst.toList());
    }
}


public final class AppendFunction
            extends Function
{
    public final static String id
    = "$Id$";


    public final static AppendFunction INSTANCE
    = new AppendFunction();

    protected Arity getArity()
    {
        return Arity.atLeast(0);
    }

    public Code call(Registers state, List arguments)
    throws RuntimeError, TypeError
    {
        return AppendHelper2.INSTANCE.reduceRight(arguments).getLiteral();
    }
}
