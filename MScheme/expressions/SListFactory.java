package MScheme.expressions;


public class SListFactory
{
    private SPair _first;
    private SPair _last;


    public SListFactory()
    {
        _first = null;
    }


    public void append(SExpr sexpr)
    {
        SPair pair = new SPair(sexpr, SEmpty.INSTANCE);

        if (_first == null) {
            _first = pair;
        } else {
            _last.setCdr(pair);
        }
        _last = pair;
    }


    public SList getList()
    {
        if (_first == null) {
            return SEmpty.INSTANCE;
        } else {
            SList result = _first;

            _first = null;

            return result;
        }
    }
}
