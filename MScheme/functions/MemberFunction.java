package MScheme.functions;

import MScheme.Value;


public class MemberFunction
    extends MemberBase
{
    public final static String id
        = "$Id$";

    public final static MemberFunction INSTANCE
        = new MemberFunction();

    protected boolean equalityPredicate(Value fst, Value snd)
    { return fst.equal(snd); }
}

