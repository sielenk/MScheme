package MScheme.functions;

import MScheme.util.Arity;

import MScheme.machine.Registers;
import MScheme.code.Code;
import MScheme.Value;
import MScheme.values.List;
import MScheme.values.Empty;
import MScheme.values.Pair;
import MScheme.values.Function;

import MScheme.exceptions.*;


final class AppendHelper1
    extends Reducer
{
    AppendHelper1(Value initial)
    { super(initial); }

    protected Value combine(Value fst, Value snd)
    { return Pair.create(fst, snd); }
}


final class AppendHelper2
    extends Reducer
{
    final static AppendHelper2 INSTANCE
        = new AppendHelper2();

    private AppendHelper2()
    { super(Empty.create()); }

    protected Value combine(Value fst, Value snd)
        throws RuntimeError, TypeError
    { return new AppendHelper1(snd).foldRight(fst.toList()); }
}


public final class AppendFunction
    extends Function
{
    public final static AppendFunction INSTANCE
        = new AppendFunction();

    public Code call(Registers registers, List arguments)
        throws RuntimeError, TypeError
    { return AppendHelper2.INSTANCE.reduceRight(arguments).getLiteral(); }
}
