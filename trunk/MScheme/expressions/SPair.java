package MScheme.expressions;

import MScheme.exceptions.SImproperListException;

public class SPair extends SList 
{
    private SExpr _car;
    private SExpr _cdr;

    public SPair(SExpr car, SExpr cdr)
    {
	_car = car;
	_cdr = cdr;
    }

    public SValues toValues()
	throws SImproperListException
    {
	SValuesFactory fab = new SValuesFactory();

	SPair pair = this;
	SExpr cdr;
	for (;;) {
	    fab.append(pair.getCar());

	    cdr = pair.getCdr();

	    if (cdr instanceof SPair) {
		pair = (SPair)cdr;
	    } else {
		break;
	    }
	}

	if (cdr == SEmpty.INSTANCE) {
	    return fab.getValues();
	} else {
	    throw new SImproperListException(this);
	}
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

