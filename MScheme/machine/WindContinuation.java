package MScheme.machine;

import MScheme.code.Code;
import MScheme.values.ValueFactory;
import MScheme.values.Value;
import MScheme.values.Function;

import MScheme.exceptions.SchemeException;


class CallThunkContinuation
    extends Continuation
{
    final private Function _thunk;

    CallThunkContinuation(Machine machine, Function thunk)
    { super(machine); _thunk = thunk; }

    protected Code internalInvoke(
        Machine machine,
        Value   value
    ) throws SchemeException
    { return _thunk.call(machine, ValueFactory.createList()); }
}


class WindContinuation
    extends Continuation
{
    final private Function _before;
    final private Function _after;

    private WindContinuation(
        Machine machine,
        Function before,
        Function after
    )
    {
        super(machine);
        _before = before;
        _after  = after;
    }


    static Code handle(
        Machine machine,
        Function before,
        Function thunk,
        Function after
    ) throws SchemeException
    {
        new WindContinuation(machine, before, after);
        new CallThunkContinuation(machine, thunk);

        return before.call(machine, ValueFactory.createList());
    }

    protected void leave(Machine machine)
    { new CallThunkContinuation(machine, _after); }

    protected void enter(Machine machine)
    { new CallThunkContinuation(machine, _before); }

    protected Code internalInvoke(
        Machine machine,
        Value   value
    ) throws SchemeException
    {
        new ValueContinuation(machine, value);
        return _after.call(machine, ValueFactory.createList());
    }
}

