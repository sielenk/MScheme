package MScheme.functions;

import MScheme.machine.State;
import MScheme.code.Code;
import MScheme.values.Value;

import MScheme.exceptions.RuntimeError;
import MScheme.exceptions.TypeError;


abstract public class TernaryValueFunction
    extends TernaryFunction
{
    final protected Code checkedCall(
        State state,
        Value fst,
        Value snd,
        Value trd
    ) throws RuntimeError, TypeError
    { return checkedCall(fst, snd, trd).getLiteral(); }

    abstract protected Value checkedCall(Value fst, Value snd, Value trd)
        throws RuntimeError, TypeError;
}
