package MScheme.functions;

import MScheme.machine.Machine;
import MScheme.code.Code;
import MScheme.values.Value;

import MScheme.exceptions.RuntimeError;
import MScheme.exceptions.TypeError;


abstract public class BinaryValueFunction
    extends BinaryFunction
{
    final protected Code checkedCall(
        Machine machine,
        Value   fst,
        Value   snd
    ) throws RuntimeError, TypeError
    { return checkedCall(fst, snd).getLiteral(); }

    abstract protected Value checkedCall(Value fst, Value snd)
        throws RuntimeError, TypeError;
}
