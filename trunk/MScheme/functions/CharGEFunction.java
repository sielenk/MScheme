package MScheme.functions;

import MScheme.values.Value;

import MScheme.exceptions.CharExpectedException;


public class CharGEFunction
    extends BinaryPredicate
{
    public final static CharGEFunction INSTANCE
        = new CharGEFunction();
    
    
    protected boolean test(
        Value fst,
        Value snd
    ) throws CharExpectedException
    { return fst.toChar().getChar() >= snd.toChar().getChar(); }
}

