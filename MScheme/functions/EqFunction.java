package MScheme.functions;

import MScheme.values.Value;


public class EqFunction
    extends BinaryPredicate
{
    public final static EqFunction INSTANCE
        = new EqFunction();
    
    
    protected boolean test(
        Value fst,
        Value snd
    )
    { return fst.eq(snd); }
}

