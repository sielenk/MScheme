package MScheme.functions;

import MScheme.machine.Registers;
import MScheme.code.Code;
import MScheme.Value;

import MScheme.exceptions.RuntimeError;
import MScheme.exceptions.TypeError;


abstract public class TernaryValueFunction
    extends TernaryFunction
{
    final protected Code checkedCall(
        Registers registers,
        Value     fst,
        Value     snd,
        Value     trd
    ) throws RuntimeError, TypeError
    { return checkedCall(fst, snd, trd).getLiteral(); }

    abstract protected Value checkedCall(Value fst, Value snd, Value trd)
        throws RuntimeError, TypeError;
}
