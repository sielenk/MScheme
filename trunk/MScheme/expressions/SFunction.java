package MScheme.expressions;


import MScheme.exceptions.SException;

import MScheme.util.Values;
import MScheme.machine.Machine;

import MScheme.environment.Environment;


public abstract class SFunction extends SExpr
{
    abstract public Values call(
        Machine machine,
        Values  arguments
    ) throws SException;


    abstract public int getMinArity();
    abstract public int getMaxArity();
}
