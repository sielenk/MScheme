package MScheme.functions;

import MScheme.values.Value;
import MScheme.values.Pair;

import MScheme.exceptions.SchemeException;


public class SetCdrFunction
    extends BinaryValueFunction
{
    public final static SetCdrFunction INSTANCE
        = new SetCdrFunction();


    protected Value checkedCall(
        Value fst,
        Value snd
    ) throws SchemeException
    {
        fst.toPair().setSecond(snd);
        return snd;
    }
}

