package MScheme.functions;

import MScheme.Value;


public class AssocFunction
    extends AssocBase
{
    public final static String id
        = "$Id$";

    public final static AssocFunction INSTANCE
        = new AssocFunction();

    protected boolean equal(Value fst, Value snd)
    { return fst.equal(snd); }
}

