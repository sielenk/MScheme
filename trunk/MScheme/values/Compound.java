package MScheme.values;

import MScheme.Value;

import MScheme.environment.StaticEnvironment;
import MScheme.code.*;

import MScheme.exceptions.*;


/**
 * 
 */
abstract class Compound
    extends ValueDefaultImplementations
{
    /** The CVS id of the file containing this class. */
    public final static String id
        = "$Id$";


    private final boolean _isConst;

    protected Compound(boolean isConst)
    {
        _isConst = isConst;
    }

    protected final void modify()
        throws ImmutableException
    {
        if (_isConst)
        {
            throw new ImmutableException(this);
        }
    }

    protected abstract Value getConstCopy();


    public final Value getConst()
    {
        return _isConst ? this : getConstCopy();
    }
}
