package MScheme.functions;

import MScheme.values.Value;
import MScheme.values.InputPort;

import MScheme.exceptions.PortExpectedException;


public class IsCharReadyFunction
    extends UnaryPredicate
{
    public final static IsCharReadyFunction INSTANCE
        = new IsCharReadyFunction();

    protected boolean test(Value value)
        throws PortExpectedException
    { return value.toPort().toInput().isReady(); }
}
