package MScheme.functions;

import MScheme.values.Value;
import MScheme.values.ValueFactory;


public class ConsFunction
    extends BinaryValueFunction
{
    public final static ConsFunction INSTANCE
        = new ConsFunction();


    protected Value checkedCall(
        Value fst,
        Value snd
    )
    { return ValueFactory.createPair(fst, snd); }
}

