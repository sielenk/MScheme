package MScheme.functions;

import MScheme.values.Value;

import MScheme.exceptions.CharExpectedException;


public class CharEQFunction
    extends BinaryPredicate
{
    public final static CharEQFunction INSTANCE
        = new CharEQFunction();
    
    
    protected boolean test(
        Value fst,
        Value snd
    ) throws CharExpectedException
    { return fst.toChar().getChar() == snd.toChar().getChar(); }
}

