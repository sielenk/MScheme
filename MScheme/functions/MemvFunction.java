package MScheme.functions;

import MScheme.Value;


public class MemvFunction
    extends MemberBase
{
    public final static String id
        = "$Id$";

    public final static MemvFunction INSTANCE
        = new MemvFunction();

    protected boolean equalityPredicate(Value fst, Value snd)
    { return fst.eqv(snd); }
}

