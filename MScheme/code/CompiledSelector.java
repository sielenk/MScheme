package MScheme.code;

import MScheme.values.Value;
import MScheme.machine.Machine;
import MScheme.machine.Continuation;


final public class CompiledSelector
    extends Code
{
    final private Code _flag;
    final private Code _onTrue;
    final private Code _onFalse;

    public CompiledSelector(
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
        SelectContinuation(Machine machine)
        { super(machine); }

        protected Code execute(Machine machine, Value evaluationResult)
        { return evaluationResult.isFalse() ? _onFalse : _onTrue; }
    }
    
    public Code executionStep(Machine machine)
    {
        new SelectContinuation(machine);
        return _flag;
    }
}
