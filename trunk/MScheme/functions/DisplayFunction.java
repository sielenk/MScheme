package MScheme.functions;

import MScheme.values.*;

import MScheme.exceptions.SchemeException;


public class DisplayFunction
    extends BinaryFunction
{
    public final static DisplayFunction INSTANCE
        = new DisplayFunction();
    
    protected Value checkedCall(
        Value fst,
        Value snd
    ) throws SchemeException
    {
        snd.toPort().toOutput().display(fst);
        return fst;
    }
}

