package MScheme.util;


import MScheme.expressions.SExpr;
import MScheme.expressions.SList;
import MScheme.expressions.SPair;


public class ListFactory
{
    private SPair _first;
    private SPair _last;


    public ListFactory()
    {
        _first = null;
    }


    public ListFactory prepend(SExpr sexpr)
    {
        SPair pair = new SPair(sexpr, SList.EMPTY);

        if (_first == null) {
            _last = pair;
        } else {
            pair.setCdr(_first);
        }
        _first = pair;

        return this;
    }


    public ListFactory append(SExpr sexpr)
    {
        SPair pair = new SPair(sexpr, SList.EMPTY);

        if (_first == null) {
            _first = pair;
        } else {
            _last.setCdr(pair);
        }
        _last = pair;

        return this;
    }


    public SList getList()
    {
        if (_first == null) {
            return SList.EMPTY;
        } else {
            SList result = _first;

            _first = null;

            return result;
        }
    }
}
