package MScheme.values;

import MScheme.Value;

import MScheme.machine.Machine;
import MScheme.environment.StaticEnvironment;
import MScheme.code.*;

import MScheme.exceptions.*;


public abstract class Compound
    extends Value
{
    private boolean _isConst = false;

    public void setConst()
    { _isConst = true; }

    protected void modify()
        throws ImmutableException
    {
        if (_isConst) {
            throw new ImmutableException(this);
        }                                    
    }
}
