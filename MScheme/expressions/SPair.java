package MScheme.expressions;

public class SPair extends SList 
{
    private SExpr _car;
    private SExpr _cdr;

    public SPair(SExpr car, SExpr cdr)
    {
	_car = car;
	_cdr = cdr;
    }

    public SExpr getCar()
    {
	return _car;
    }

    public SExpr getCdr()
    {
	return _cdr;
    }
}

