package MScheme.expressions;


import MScheme.machine.Values;


public class SEmpty extends SList
{
    public final static SEmpty INSTANCE = new SEmpty();

    private SEmpty() { }

    public Values toValues()
    { return Values.EMPTY; }
}
