package MScheme.functions;

import MScheme.values.Value;


public class EqualFunction
    extends BinaryPredicate
{
    public final static EqualFunction INSTANCE
        = new EqualFunction();
    
    
    protected boolean test(
        Value fst,
        Value snd
    )
    { return fst.equal(snd); }
}

