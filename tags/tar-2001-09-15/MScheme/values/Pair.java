package MScheme.values;

import MScheme.Value;
import MScheme.Code;

import MScheme.machine.Machine;
import MScheme.environment.StaticEnvironment;
import MScheme.code.CodeList;

import MScheme.exceptions.*;


public abstract class Pair
    extends List
{
    public final static String id
        = "$Id$";


    protected Pair()
    { }

    public static Pair create(Value first, Value second)
    { return new MutablePair(first, second); }

    public static Pair createConst(Value first, Value second)
    { return new ConstPair(first.getConst(), second.getConst()); }


    public final boolean isPair()
    { return true; }
    
    public final Pair toPair()
    { return this; }


    public final boolean equal(Value other)
    {
        try {
            Pair otherPair = (Pair)other;
        
            return
                (getFirst ().equal(otherPair.getFirst ())) &&
                (getSecond().equal(otherPair.getSecond()));
        }
        catch (ClassCastException e) { }
        
        return false;
    }
    

    // implementation of List
    
    public final boolean isEmpty()
    { return false; }

    public final boolean isList()
    { return getSecond().isList(); }

    public final int safeGetLength()
    {
        int   result = 1;
        
        Value tail = getSecond();
        for (
            ;
            tail instanceof Pair;
            tail = ((Pair)tail).getSecond()
        ) {
            ++result;
        }

        return tail.isList() ? result : -result;
    }

    public final Value getHead()
    { return getFirst(); }
    
    public final List getTail()
        throws ListExpected
    { return getSecond().toList(); }

    public final Code getCode(StaticEnvironment compilationEnv)
        throws CompileError, TypeError
    {
        return
            getHead()
            .getTranslator(compilationEnv)
            .translate(
                compilationEnv,
                getTail()
            );
    }

    public final CodeList getCodeList(StaticEnvironment compilationEnv)
        throws CompileError, TypeError
    {
        return CodeList.prepend(
            getHead().getCode    (compilationEnv),
            getTail().getCodeList(compilationEnv)
        );
    }


    // abstract interface of Pair
    
    public abstract Value getFirst();
    
    public abstract void setFirst(Value first)
        throws ImmutableException;
    
    public abstract Value getSecond();

    public abstract void setSecond(Value second)
        throws ImmutableException;
}


final class ConstPair
    extends Pair
{
    public final static String id
        = "$Id";


    private final Value _first;
    private final Value _second;


    ConstPair(Value first, Value second)
    {
        _first  = first;
        _second = second;
    }


    // implementation of Pair
    
    public Value getFirst()
    { return _first; }
    
    public void setFirst(Value first)
        throws ImmutableException
    { throw new ImmutableException(this); }
    
    public Value getSecond()
    { return _second; }    

    public void setSecond(Value second)
        throws ImmutableException
    { throw new ImmutableException(this); }
}


final class MutablePair
    extends Pair
{
    public final static String id
        = "$Id$";


    private Value _first;
    private Value _second;


    MutablePair(Value first, Value second)
    {
        _first  = first;
        _second = second;
    }


    // specialisation of Value

    public Value getConst()
    { return createConst(_first, _second); }


    // implementation of Pair

    public Value getFirst()
    { return _first; }

    public void setFirst(Value first)
        throws ImmutableException
    { _first = first; }

    public Value getSecond()
    { return _second; }

    public void setSecond(Value second)
    { _second = second; }
}
