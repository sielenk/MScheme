package MScheme.functions;

import MScheme.Value;
import MScheme.Code;

import MScheme.machine.Registers;

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
