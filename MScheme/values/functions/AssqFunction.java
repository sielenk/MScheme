package MScheme.values.functions;

import MScheme.Value;


public final class AssqFunction
            extends AssocBase
{
    public final static String id
    = "$Id$";


    public final static AssqFunction INSTANCE
    = new AssqFunction();

    protected boolean equal(Value fst, Value snd)
    {
        return fst.eq(snd);
    }
}
