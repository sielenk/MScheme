package MScheme.functions;

import MScheme.machine.Machine;
import MScheme.code.Code;
import MScheme.values.Value;

import MScheme.exceptions.RuntimeError;
import MScheme.exceptions.TypeError;


abstract public class ValueThunk
    extends Thunk
{
    final protected Code checkedCall(
        Machine machine
    ) throws RuntimeError, TypeError
    { return machine.handleResult(checkedCall()); }

    abstract protected Value checkedCall()
        throws RuntimeError, TypeError;
}
