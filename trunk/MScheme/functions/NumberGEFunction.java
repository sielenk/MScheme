package MScheme.functions;

import MScheme.values.SchemeNumber;


public class NumberGEFunction
    extends CompareNumbers
{
    public final static NumberGEFunction INSTANCE = new NumberGEFunction();


    protected boolean compare(
        SchemeNumber fst,
        SchemeNumber snd
    )
    { return !fst.isLessThan(snd); }
}

