package MScheme.functions;

import MScheme.machine.Registers;
import MScheme.code.Code;
import MScheme.values.Value;

import MScheme.exceptions.RuntimeError;
import MScheme.exceptions.TypeError;


abstract public class UnaryValueFunction
    extends UnaryFunction
{
    final protected Code checkedCall(
        Registers registers,
        Value     fst
    ) throws RuntimeError, TypeError
    { return checkedCall(fst).getLiteral(); }

    abstract protected Value checkedCall(Value fst)
        throws RuntimeError, TypeError;
}
