package MScheme.functions;

import MScheme.values.SchemeNumber;


public class NumberGTFunction
    extends CompareNumbers
{
    public final static NumberGTFunction INSTANCE = new NumberGTFunction();


    protected boolean compare(
        SchemeNumber fst,
        SchemeNumber snd
    )
    { return snd.isLessThan(fst); }
}

