package MScheme.functions;

import MScheme.values.Value;

import MScheme.exceptions.CharExpectedException;


public class CharLTFunction
    extends BinaryPredicate
{
    public final static CharLTFunction INSTANCE
        = new CharLTFunction();
    
    
    protected boolean test(
        Value fst,
        Value snd
    ) throws CharExpectedException
    { return fst.toChar().getChar() < snd.toChar().getChar(); }
}

