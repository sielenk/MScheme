package MScheme.functions;

import MScheme.Value;


public final class AssvFunction
    extends AssocBase
{
    public final static String id
        = "$Id$";


    public final static AssvFunction INSTANCE
        = new AssvFunction();

    protected boolean equal(Value fst, Value snd)
    { return fst.eqv(snd); }
}