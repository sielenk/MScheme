package MScheme.machine;


import MScheme.expressions.SExpr;


final class ValuePair
{
    SExpr     head;
    ValuePair tail;


    ValuePair(SExpr h)
    { head = h; }
}
