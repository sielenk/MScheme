package MScheme.machine;

import MScheme.code.Code;
import MScheme.values.Value;
import MScheme.values.Empty;
import MScheme.values.Function;

import MScheme.exceptions.*;


class CallThunkContinuation
    extends Continuation
{
    final private Function _thunk;

    CallThunkContinuation(Machine machine, Function thunk)
    { super(machine); _thunk = thunk; }

    protected Code execute(
        Machine machine,
        Value   value
    ) throws RuntimeError, TypeError
    { return _thunk.call(machine, Empty.create()); }
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
    ) throws RuntimeError, TypeError
    {
        new WindContinuation(machine, before, after);
        new CallThunkContinuation(machine, thunk);

        return before.call(machine, Empty.create());
    }

    protected void leave(Machine machine)
    { new CallThunkContinuation(machine, _after); }

    protected void enter(Machine machine)
    { new CallThunkContinuation(machine, _before); }

    protected Code execute(
        Machine machine,
        Value   value
    ) throws RuntimeError, TypeError
    {
        new ValueContinuation(machine, value);
        return _after.call(machine, Empty.create());
    }
}
