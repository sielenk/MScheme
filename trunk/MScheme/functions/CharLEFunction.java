package MScheme.functions;

import MScheme.values.Value;

import MScheme.exceptions.CharExpectedException;


public class CharLEFunction
    extends BinaryPredicate
{
    public final static CharLEFunction INSTANCE
        = new CharLEFunction();
    
    
    protected boolean test(
        Value fst,
        Value snd
    ) throws CharExpectedException
    { return fst.toChar().getChar() <= snd.toChar().getChar(); }
}

