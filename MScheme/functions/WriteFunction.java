package MScheme.functions;

import MScheme.values.*;

import MScheme.exceptions.SchemeException;


public class WriteFunction
    extends BinaryValueFunction
{
    public final static WriteFunction INSTANCE
        = new WriteFunction();
    
    protected Value checkedCall(
        Value fst,
        Value snd
    ) throws SchemeException
    {
        snd.toPort().toOutput().write(fst);
        return fst;
    }
}

