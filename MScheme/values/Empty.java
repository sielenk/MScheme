package MScheme.values;

import java.io.Writer;
import java.io.IOException;

import MScheme.Value;
import MScheme.Code;

import MScheme.environment.StaticEnvironment;
import MScheme.code.CodeList;
import MScheme.exceptions.*;


public final class Empty
    extends List
{
    public final static String id
        = "$Id$";


    private final static Empty INSTANCE = new Empty();

    private Empty()
    { }

    public static Empty create()
    { return INSTANCE; }


    // implementation of List
    
    public boolean isEmpty()
    { return true; }
    
    public boolean isList()
    { return true; }

    public int safeGetLength()
    { return 0; }
    
    public Value getHead()
        throws PairExpected
    { throw new PairExpected(this);  }
    
    public List getTail()
        throws PairExpected
    { throw new PairExpected(this); }
    
    public Code getCode(StaticEnvironment e)
        throws CantCompileException
    { throw new CantCompileException(this); }
    
    public CodeList getCodeList(StaticEnvironment e)
    { return CodeList.create(); }    
}
