package MScheme.expressions.functions;

import MScheme.expressions.SExpr;
import MScheme.exceptions.SException;
import MScheme.machine.ContinuationStack;
import MScheme.environment.Environment;

public abstract class Function extends SExpr
{

    abstract public SExpr call(
	ContinuationStack stack,
	Environment environment,
	SExpr sexpr
    ) throws SException;
}
