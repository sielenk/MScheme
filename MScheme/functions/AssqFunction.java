package MScheme.functions;

import MScheme.values.Value;


public class AssqFunction
    extends AssocBase
{
    public final static AssqFunction INSTANCE
        = new AssqFunction();

    protected boolean equal(Value fst, Value snd)
    { return fst.eq(snd); }
}

