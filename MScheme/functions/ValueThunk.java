package MScheme.functions;

import MScheme.machine.Registers;
import MScheme.Code;
import MScheme.Value;

import MScheme.exceptions.RuntimeError;
import MScheme.exceptions.TypeError;


abstract public class ValueThunk
    extends Thunk
{
    public final static String id
        = "$Id$";

    final protected Code checkedCall(
        Registers registers
    ) throws RuntimeError, TypeError
    { return checkedCall().getLiteral(); }

    abstract protected Value checkedCall()
        throws RuntimeError, TypeError;
}
