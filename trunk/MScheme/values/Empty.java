package MScheme.values;

import MScheme.environment.StaticEnvironment;
import MScheme.values.Value;
import MScheme.code.*;
import MScheme.exceptions.*;


public final class Empty
    extends List
{
    final static Empty INSTANCE  = new Empty();
    
    private Empty() { }


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
    
    public int getLength()
    { return 0; }
    
    public Value getHead()
        throws PairExpectedException
    { throw new PairExpectedException(this);  }
    
    public List getTail()
        throws PairExpectedException
    { throw new PairExpectedException(this); }
    
    public List getReversed()
    { return this; }
}

