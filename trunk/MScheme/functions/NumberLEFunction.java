package MScheme.functions;

import MScheme.values.SchemeNumber;


public class NumberLEFunction
    extends CompareNumbers
{
    public final static NumberLEFunction INSTANCE = new NumberLEFunction();


    protected boolean compare(
        SchemeNumber fst,
        SchemeNumber snd
    )
    { return !snd.isLessThan(fst); }
}

