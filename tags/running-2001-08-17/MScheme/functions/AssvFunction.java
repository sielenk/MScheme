package MScheme.functions;

import MScheme.values.Value;


public class AssvFunction
    extends AssocBase
{
    public final static AssvFunction INSTANCE
        = new AssvFunction();

    protected boolean equal(Value fst, Value snd)
    { return fst.eqv(snd); }
}

