package MScheme.expressions;


import MScheme.exceptions.SException;

import MScheme.machine.Values;
import MScheme.machine.ContinuationStack;

import MScheme.environment.Environment;


public abstract class SFunction extends SExpr
{
    abstract public Values call(
        ContinuationStack stack,
        Environment       environment,
        Values            arguments
    ) throws SException;

    abstract public int getMinArity();
    abstract public int getMaxArity();
}
