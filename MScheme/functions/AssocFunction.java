package MScheme.functions;

import MScheme.values.Value;


public class AssocFunction
    extends AssocBase
{
    public final static AssocFunction INSTANCE
        = new AssocFunction();

    protected boolean equal(Value fst, Value snd)
    { return fst.equal(snd); }
}

