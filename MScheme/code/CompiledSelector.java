package MScheme.code;

import MScheme.machine.Machine;


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
    
    public Code executionStep(Machine machine)
    {
        machine.select(_onTrue, _onFalse);
        return _flag;
    }
}
