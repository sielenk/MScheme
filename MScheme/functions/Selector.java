package MScheme.functions;

import MScheme.machine.Machine;
import MScheme.code.Code;
import MScheme.values.Value;


final public class Selector
    extends UnaryFunction
{
    private final Code _onTrue;
    private final Code _onFalse;

    public Selector(Code onTrue, Code onFalse)
    { _onTrue = onTrue; _onFalse = onFalse; }

    protected Code checkedCall(Machine machine, Value flag)
    { return flag.isFalse() ? _onFalse : _onTrue; }
}

