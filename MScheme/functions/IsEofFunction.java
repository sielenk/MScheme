package MScheme.functions;

import MScheme.values.Value;
import MScheme.values.InputPort;


public class IsEofFunction
    extends UnaryPredicate
{
    public final static IsEofFunction INSTANCE
        = new IsEofFunction();

    protected boolean test(Value value)
    { return value.eq(InputPort.EOF_VALUE); }
}

