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
}
