package MScheme.expressions;


import MScheme.machine.Values;
import MScheme.exceptions.SImproperListException;


abstract public class SList extends SExpr
{
    abstract public Values toValues()
        throws SImproperListException;
}
