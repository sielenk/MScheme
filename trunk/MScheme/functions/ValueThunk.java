package MScheme.functions;

import MScheme.machine.State;
import MScheme.code.Code;
import MScheme.values.Value;

import MScheme.exceptions.RuntimeError;
import MScheme.exceptions.TypeError;


abstract public class ValueThunk
    extends Thunk
{
    final protected Code checkedCall(
        State state
    ) throws RuntimeError, TypeError
    { return checkedCall().getLiteral(); }

    abstract protected Value checkedCall()
        throws RuntimeError, TypeError;
}
