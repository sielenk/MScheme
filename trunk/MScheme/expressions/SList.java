package MScheme.expressions;

import MScheme.exceptions.SImproperListException;

abstract public class SList extends SExpr
{
    abstract public SValues toValues()
	throws SImproperListException;
}
