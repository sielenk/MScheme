package MScheme.functions;

import MScheme.Value;


public final class MemberFunction
    extends MemberBase
{
    public final static String id
        = "$Id$";


    public final static MemberFunction INSTANCE
        = new MemberFunction();

    protected boolean equal(Value fst, Value snd)
    { return fst.equal(snd); }
}
