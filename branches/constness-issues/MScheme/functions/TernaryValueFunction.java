package MScheme.functions;

import MScheme.machine.Registers;
import MScheme.Code;
import MScheme.Value;

import MScheme.exceptions.RuntimeError;
import MScheme.exceptions.TypeError;


public abstract class TernaryValueFunction
    extends TernaryFunction
{
    public final static String id
        = "$Id$";

    protected final Code checkedCall(
        Registers registers,
        Value     fst,
        Value     snd,
        Value     trd
    ) throws RuntimeError, TypeError
    { return checkedCall(fst, snd, trd).getLiteral(); }

    protected abstract Value checkedCall(Value fst, Value snd, Value trd)
        throws RuntimeError, TypeError;
}
