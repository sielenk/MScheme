package MScheme.functions;

import MScheme.Value;
import MScheme.Code;

import MScheme.machine.Registers;

import MScheme.exceptions.RuntimeError;
import MScheme.exceptions.TypeError;


public abstract class UnaryValueFunction
    extends UnaryFunction
{
    public final static String id
        = "$Id$";

    protected final Code checkedCall(
        Registers registers,
        Value     fst
    ) throws RuntimeError, TypeError
    { return checkedCall(fst).getLiteral(); }

    protected abstract Value checkedCall(Value fst)
        throws RuntimeError, TypeError;
}
