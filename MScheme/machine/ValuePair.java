package MScheme.machine;


import MScheme.expressions.SExpr;


public final class ValuePair
{
    public SExpr     head;
    public ValuePair tail;


    public ValuePair(SExpr h)
    { head = h; }

    public ValuePair(SExpr h, ValuePair t)
    { head = h; tail = t; }
}
