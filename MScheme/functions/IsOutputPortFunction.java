package MScheme.functions;

import MScheme.values.Value;
import MScheme.exceptions.PortExpectedException;


public class IsOutputPortFunction
    extends UnaryPredicate
{
    public final static IsOutputPortFunction INSTANCE
        = new IsOutputPortFunction();

    protected boolean test(Value value)
        throws PortExpectedException
    {
        return
            value.isPort()
            && value.toPort().isOutput();
    }
}

