package MScheme.functions;

import MScheme.machine.Registers;
import MScheme.Code;
import MScheme.Value;

import MScheme.exceptions.RuntimeError;
import MScheme.exceptions.TypeError;


public abstract class ValueThunk
    extends Thunk
{
    public final static String id
        = "$Id$";

    protected final Code checkedCall(
        Registers state
    ) throws RuntimeError, TypeError
    { return checkedCall().getLiteral(); }

    protected abstract Value checkedCall()
        throws RuntimeError, TypeError;
}
