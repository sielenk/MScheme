package MScheme.values;

import MScheme.Value;
import MScheme.List;


public abstract class ListFactory
{
    public final static String id
        = "$Id$";


    public static List prepend(Value head, List tail)
    { return PairOrList.prepend(head, tail); }

    public static List create()
    { return Empty.create(); }

    public static List create(Value first)
    { return prepend(first, create()); }

    public static List create(Value first, Value second)
    { return prepend(first, create(second)); }

    public static List create(Value first, Value second, Value third)
    { return prepend(first, create(second, third)); }
}
