package MScheme.machine;


import MScheme.expressions.SExpr;
import MScheme.expressions.SList;
import MScheme.expressions.SListFactory;


public class Values
{
    private ValuePair _data;
    private int       _length;


    public final static Values EMPTY = new Values(null, 0);


    public Values(SExpr sexpr)
    {
        _data      = new ValuePair(sexpr);
        _data.tail = null;
        _length    = 1;
    }


    Values(ValuePair data, int length)
    {
        _data   = data;
        _length = length;
    }


    public int getLength()
    {
        return _length;
    }


    public SExpr getFirst()
    {
        return _data.head;
    }


    public Values getTail()
    {
        if (_length > 1) {
            return new Values(_data.tail, _length - 1);
        } else {
            return EMPTY;
        }
    }


    public SList toList()
    {
        ValuePair pair = _data;
        SListFactory fab = new SListFactory();

        while (pair != null) {
            fab.append(pair.head);
            pair = pair.tail;
        }

        return fab.getList();
    }
}
