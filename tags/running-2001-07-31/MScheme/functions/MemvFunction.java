package MScheme.functions;

import MScheme.values.Value;


public class MemvFunction
    extends MemberBase
{
    public final static MemvFunction INSTANCE
        = new MemvFunction();

    protected boolean equal(Value fst, Value snd)
    { return fst.eqv(snd); }
}
