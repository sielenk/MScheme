package MScheme.values;

import MScheme.Value;
import MScheme.Code;

import MScheme.machine.Machine;
import MScheme.environment.StaticEnvironment;
import MScheme.code.CodeList;

import MScheme.exceptions.*;


public final class Pair
    extends List
{
    public final static String id
        = "$Id$";


    public static Pair create(Value first, Value second)
    { return new Pair(false, first, second); }

    public static Pair createConst(Value first, Value second)
    { return new Pair(true, first.getConst(), second.getConst()); }


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


    // implementation of Compound

    protected Value getConstCopy()
    { return createConst(getFirst(), getSecond()); }


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


    // implementation of Pair

    private Value _first;
    private Value _second;


    private Pair(boolean isConst, Value first, Value second)
    {
        super(isConst);

        _first  = first;
        _second = second;
    }

    public Value getFirst()
    { return _first; }

    public void setFirst(Value first)
        throws ImmutableException
    {
        modify();
        _first = first;
    }

    public Value getSecond()
    { return _second; }

    public void setSecond(Value second)
        throws ImmutableException
    {
        modify();
        _second = second;
    }
}
