package MScheme.values;

import MScheme.Value;


public abstract class ListFactory
{
    public final static String id
        = "$Id$";


    // *** List creation ***

    public static List prepend(Value head, List tail)
    {
        return PairOrList.prepend(head, tail);
    }

    public static List create()
    {
        return Empty.create();
    }

    public static List create(Value first)
    {
        return prepend(first, create());
    }

    public static List create(Value first, Value second)
    {
        return prepend(first, create(second));
    }

    public static List create(Value first, Value second, Value third)
    {
        return prepend(first, create(second, third));
    }

    public static List create(
        Value first,
        Value second,
        Value third,
        Value fourth
    )
    {
        return prepend(first, create(second, third, fourth));
    }


    // *** Pair creation ***
    
    public static Pair createPair(Value fst, Value snd)
    {
        return PairOrList.create(fst, snd);
    }
 
    public static Pair createConstPair(Value fst, Value snd)
    {
        return PairOrList.createConst(fst, snd);
    }
}
