package MScheme.functions;

import MScheme.values.Value;

import MScheme.exceptions.CharExpectedException;


public class CharGTFunction
    extends BinaryPredicate
{
    public final static CharGTFunction INSTANCE
        = new CharGTFunction();
    
    
    protected boolean test(
        Value fst,
        Value snd
    ) throws CharExpectedException
    { return fst.toChar().getChar() > snd.toChar().getChar(); }
}

