package MScheme.util;


import MScheme.expressions.SExpr;
import MScheme.expressions.SList;


public class Values
{
    private SExpr[] _data;
    private int     _first;


    public final static Values EMPTY = new Values(new SExpr[0], 0);


    private Values(
        SExpr[] data,
        int     first
    ) {
        _data  = data;
        _first = first;
    }


    Values(SExpr[] data)
    {
        this(data, 0);
    }


    public Values(SExpr sexpr)
    {
        this(new SExpr[1]);
        _data[0] = sexpr;
    }


    public int getLength()
    {
        return _data.length - _first;
    }


    public Values getTail(int index)
    {
        int newFirst = _first + index;

        return
            (newFirst < _data.length)
            ? new Values(_data, newFirst)
            : EMPTY;
    }


    public Values getTail()
    {
        return getTail(1);
    }


    public SExpr at(int index)
    {
        return _data[_first + index];
    }


    public SList toList()
    {
        ListFactory fab = new ListFactory();

        for (int i = _first; i < _data.length; i++) {
            fab.append(
                _data[i]
            );
        }

        return fab.getList();
    }
}
