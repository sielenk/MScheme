package MScheme.functions;

import MScheme.Value;


public class MemqFunction
    extends MemberBase
{
    public final static String id
        = "$Id$";

    public final static MemqFunction INSTANCE
        = new MemqFunction();

    protected boolean equal(Value fst, Value snd)
    { return fst.eq(snd); }
}