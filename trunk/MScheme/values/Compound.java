package MScheme.values;

import MScheme.Value;

import MScheme.machine.Machine;
import MScheme.environment.StaticEnvironment;
import MScheme.code.*;

import MScheme.exceptions.*;


public abstract class Compound
    extends Value
{
    public final static String id
        = "$Id$";


    private boolean _isMutable = true;

    public void setConst()
    { _isMutable = false; }

    protected void modify()
        throws ImmutableException
    {
        if (!_isMutable) {
            throw new ImmutableException(this);
        }                                    
    }
}
