package MScheme.functions;

import MScheme.values.Value;


public class EqvFunction
    extends BinaryPredicate
{
    public final static EqvFunction INSTANCE
        = new EqvFunction();
    
    
    protected boolean test(
        Value fst,
        Value snd
    )
    { return fst.eqv(snd); }
}

