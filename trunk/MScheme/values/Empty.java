package MScheme.values;

import java.io.Writer;
import java.io.IOException;

import MScheme.Value;
import MScheme.List;
import MScheme.Code;

import MScheme.environment.StaticEnvironment;
import MScheme.code.CodeList;
import MScheme.exceptions.*;


public final class Empty
            extends ValueDefaultImplementations
            implements List
{
    public final static String id
    = "$Id$";


    private final static Empty INSTANCE = new Empty();

    private Empty()
    { }

    public static Empty create()
    {
        return INSTANCE;
    }


    // specialisation of ValueImplementation

    public void write(Writer destination)
    throws IOException
    {
        destination.write("()");
    }

    public boolean isList()
    {
        return true;
    }

    public List toList()
    {
        return this;
    }


    // implementation of List

    public boolean isEmpty()
    {
        return true;
    }

    public int getLength()
    {
        return 0;
    }

    public List getReversed()
    {
        return this;
    }

    public Value getHead()
    throws PairExpected
    {
        throw new PairExpected(this);
    }

    public List getTail()
    throws PairExpected
    {
        throw new PairExpected(this);
    }

    public Code getCode(StaticEnvironment compilationEnv)
    throws CantCompileException
    {
        throw new CantCompileException(this);
    }

    public CodeList getCodeList(StaticEnvironment compilationEnv)
    {
        return CodeList.create();
    }
}
