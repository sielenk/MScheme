package MScheme.functions;

import MScheme.util.Arity;

import MScheme.values.ValueFactory;
import MScheme.values.Value;
import MScheme.values.List;
import MScheme.values.Pair;

import MScheme.exceptions.*;


class AppendHelper1
    extends Reducer
{
    private final Value _initial;

    AppendHelper1(Value initial)
    { _initial = initial; }

    protected Value initial()
    { return _initial; }

    protected Value combine(Value fst, Value snd)
    { return Pair.create(fst, snd); }
}


class AppendHelper2
    extends Reducer
{
    final static AppendHelper2 INSTANCE
        = new AppendHelper2();

    protected Value initial()
    { return ValueFactory.createList(); }

    protected Value combine(Value fst, Value snd)
        throws RuntimeError, TypeError
    { return new AppendHelper1(snd).foldRight(fst.toList()); }
}


public class AppendFunction
    extends CheckedFunction
{
    public final static AppendFunction INSTANCE
        = new AppendFunction();

    protected Value checkedCall(List arguments)
        throws RuntimeError, TypeError
    { return AppendHelper2.INSTANCE.reduceRight(arguments); }
}

