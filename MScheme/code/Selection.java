package MScheme.code;

import MScheme.values.Value;
import MScheme.machine.State;
import MScheme.machine.Continuation;


final public class Selection
    extends Code
{
    final private Code _flag;
    final private Code _onTrue;
    final private Code _onFalse;

    public Selection(
        Code flag,
        Code onTrue,
        Code onFalse
    )
    {
        _flag    = flag;
        _onTrue  = onTrue;
        _onFalse = onFalse;
    }

    class SelectContinuation
        extends Continuation
    {
        SelectContinuation(State state)
        { super(state); }

        protected Code execute(State state, Value evaluationResult)
        { return evaluationResult.isTrue() ? _onTrue : _onFalse; }
    }

    public Code executionStep(State state)
    {
        new SelectContinuation(state);
        return _flag;
    }
}
