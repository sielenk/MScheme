package MScheme.expressions;


import MScheme.util.Values;
import MScheme.exceptions.SImproperListException;


class SEmpty extends SList
{
    SEmpty() { }

    public Values toValues()
    { return Values.EMPTY; }


    protected String defaultString()
    {
        return "()";
    }
}


abstract public class SList extends SExpr
{
    public final static SList EMPTY = new SEmpty();


    abstract public Values toValues()
        throws SImproperListException;


    public static SList cons(SExpr car, SExpr cdr)
    {
        return new SPair(car, cdr);
    }
}
