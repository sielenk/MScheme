package MScheme.values;

import java.io.Writer;
import java.io.IOException;

import MScheme.environment.StaticEnvironment;
import MScheme.values.Value;
import MScheme.code.*;
import MScheme.exceptions.*;


public final class Empty
    extends    Value
    implements List
{
    private final static Empty INSTANCE = new Empty();

    private Empty() { }
    
    public static Empty create()
    { return INSTANCE; }

    // implementation of List
    
    public boolean isList()
    { return true; }

    public Value toValue()
    { return this; }

    // specialisation of Value
    
    public void write(Writer destination)
        throws IOException
    { destination.write("()"); }

    public List toList()
    { return this; }

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
    
    public int getLength()
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
