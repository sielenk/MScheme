package MScheme.functions;

import MScheme.machine.Machine;
import MScheme.code.Code;
import MScheme.values.Value;
import MScheme.values.List;
import MScheme.values.Function;

import MScheme.exceptions.RuntimeError;
import MScheme.exceptions.TypeError;


abstract public class ValueFunction
    extends Function
{
    final public Code call(
        Machine machine,
        List    arguments
    ) throws RuntimeError, TypeError
    { return machine.handleResult(call(arguments)); }

    abstract protected Value call(List arguments)
        throws RuntimeError, TypeError;
}
