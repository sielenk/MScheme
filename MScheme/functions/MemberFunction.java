package MScheme.functions;

import MScheme.Value;


public class MemberFunction
    extends MemberBase
{
    public final static MemberFunction INSTANCE
        = new MemberFunction();

    protected boolean equal(Value fst, Value snd)
    { return fst.equal(snd); }
}

