package MScheme.functions;

import MScheme.machine.Machine;
import MScheme.code.Code;
import MScheme.values.Value;

import MScheme.exceptions.SchemeException;


abstract public class BinaryValueFunction
    extends BinaryFunction
{
    final protected Code checkedCall(
        Machine machine,
        Value   fst,
        Value   snd
    ) throws SchemeException
    { return machine.handleResult(checkedCall(fst, snd)); }

    abstract protected Value checkedCall(
        Value fst,
        Value snd
    ) throws SchemeException;
}
