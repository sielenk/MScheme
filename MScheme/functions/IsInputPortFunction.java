package MScheme.functions;

import MScheme.values.Value;
import MScheme.exceptions.PortExpectedException;


public class IsInputPortFunction
    extends UnaryPredicate
{
    public final static IsInputPortFunction INSTANCE
        = new IsInputPortFunction();

    protected boolean test(Value value)
        throws PortExpectedException
    {
        return
            value.isPort()
            && value.toPort().isInput();
    }
}

