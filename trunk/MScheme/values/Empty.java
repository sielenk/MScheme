package MScheme.values;

import MScheme.environment.StaticEnvironment;
import MScheme.values.Value;
import MScheme.code.*;
import MScheme.exceptions.*;


final class Empty
    extends List
{
    Empty() { }

    // implementation of List
    
    public boolean isList()
    { return true; }


    // specialisation of Value
    
    public boolean isEmpty()
    { return true; }
    
    public Code getCode(StaticEnvironment e)
        throws CantCompileException
    { throw new CantCompileException(this); }
    
    public CodeList getCodeList(StaticEnvironment e)
    { return CodeList.create(); }
    
    // implementation of List
    
    public int safeGetLength()
    { return 0; }
    
    public Value getHead()
        throws PairExpected
    { throw new PairExpected(this);  }
    
    public List getTail()
        throws PairExpected
    { throw new PairExpected(this); }
    
    public List getReversed()
    { return this; }
}

