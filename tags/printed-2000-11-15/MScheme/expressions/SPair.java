package MScheme.expressions;


import MScheme.machine.Values;
import MScheme.machine.ValuesFactory;
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

    public Values toValues()
        throws SImproperListException
    {
        ValuesFactory fab  = new ValuesFactory();
        SPair         curr = this;

        try {
            for (;;) {
                SExpr next = curr.getCdr();

                fab.append(curr.getCar());

                if (next == SEmpty.INSTANCE) {
                    return fab.getValues();
                } else {
                    curr = (SPair)next;
                    // throws ClassCastException
                }
            }
        }
        catch (ClassCastException e) {
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

    public void setCar(SExpr car)
    {
        _car = car;
    }

    public void setCdr(SExpr cdr)
    {
        _cdr = cdr;
    }


    protected String defaultString()
    {
        StringBuffer result = new StringBuffer("(").append(_car);

        SExpr sexpr = _cdr;

        while (sexpr instanceof SPair) {
            SPair pair = (SPair)sexpr;
            result.append(" ").append(pair._car);
            sexpr = pair._cdr;
        }

        if (sexpr != SEmpty.INSTANCE) {
            result.append(" . ").append(sexpr);
        }

        result.append(")");

        return result.toString();
    }
}
