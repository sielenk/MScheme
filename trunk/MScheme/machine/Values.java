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


    public Values getTail(int index)
    {
        if ((index == 0) || (_length == 0)) {
            return this;
        } else {
            ValuePair pair   = _data;
            int       length = _length;

            do {
                pair = pair.tail;
                length--;
            } while ((--index > 0) && (length > 0));

            return
                (length > 0)
                ? new Values(pair, length)
                : EMPTY;
        }
    }


    public Values getTail() /* == getTail(1) */
    {
        return
            (_length > 1)
            ? new Values(_data.tail, _length - 1)
            : EMPTY;
    }


    public SExpr at(int index)
    {
        throw new RuntimeException("public SExpr at(int index): stub");
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
