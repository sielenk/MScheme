package MScheme.functions;

import MScheme.values.SchemeNumber;


public class NumberLTFunction
    extends CompareNumbers
{
    public final static NumberLTFunction INSTANCE = new NumberLTFunction();


    protected boolean compare(
        SchemeNumber fst,
        SchemeNumber snd
    )
    { return fst.isLessThan(snd); }
}

