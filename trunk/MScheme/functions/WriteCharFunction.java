package MScheme.functions;

import MScheme.values.*;

import MScheme.exceptions.SchemeException;


public class WriteCharFunction
    extends BinaryFunction
{
    public final static WriteCharFunction INSTANCE
        = new WriteCharFunction();
    
    protected Value checkedCall(
        Value fst,
        Value snd
    ) throws SchemeException
    {
        snd.toPort().toOutput().writeChar(
            fst.toChar().getChar()
        );
        return fst;
    }
}

