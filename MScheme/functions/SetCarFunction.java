package MScheme.functions;

import MScheme.values.Value;
import MScheme.values.Pair;

import MScheme.exceptions.SchemeException;


public class SetCarFunction
    extends BinaryValueFunction
{
    public final static SetCarFunction INSTANCE
        = new SetCarFunction();


    protected Value checkedCall(
        Value fst,
        Value snd
    ) throws SchemeException
    {
        fst.toPair().setFirst(snd);
        return snd;
    }
}

