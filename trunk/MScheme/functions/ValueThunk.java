package MScheme.functions;

import MScheme.machine.Registers;
import MScheme.code.Code;
import MScheme.Value;

import MScheme.exceptions.RuntimeError;
import MScheme.exceptions.TypeError;


abstract public class ValueThunk
    extends Thunk
{
    final protected Code checkedCall(
        Registers registers
    ) throws RuntimeError, TypeError
    { return checkedCall().getLiteral(); }

    abstract protected Value checkedCall()
        throws RuntimeError, TypeError;
}
