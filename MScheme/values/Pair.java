package MScheme.values;

import java.io.Writer;
import java.io.IOException;

import MScheme.Value;
import MScheme.Code;

import MScheme.environment.StaticEnvironment;
import MScheme.code.CodeList;

import MScheme.exceptions.*;


public abstract class Pair
            extends Compound
{
    public final static String id
    = "$Id$";


    public static Pair create(Value first, Value second)
    {
        return PairOrList.create(first, second);
    }

    public static Pair createConst(Value first, Value second)
    {
        return PairOrList.createConst(first, second);
    }


    // specialisation of ValueImplementation

    private final void put(Writer destination, boolean doDisplay)
    throws IOException
    {
        destination.write('(');

        Value current = this;
        while (current instanceof Pair)
        {
            // 'this' is the first element of the list
            // and needs no leading space
            // (the opening parenthesis is a delimiter)
            if (current != this)
            {
                destination.write(' ');
            }

            Pair currentPair = (Pair)current;

            if (doDisplay)
            {
                currentPair.getFirst().display(destination);
            }
            else
            {
                currentPair.getFirst().write(destination);
            }

            current = currentPair.getSecond();
        }

        if (!current.isEmpty())
        {
            // 'this' is an improper list

            destination.write(" . ");

            if (doDisplay)
            {
                current.display(destination);
            }
            else
            {
                current.write(destination);
            }
        }

        destination.write(')');
    }

    public final void write(Writer destination)
    throws IOException
    {
        put(destination, false);
    }

    public final void display(Writer destination)
    throws IOException
    {
        put(destination, true);
    }


    public abstract boolean isList();
    
//  public abstract List toList()
//  throws ListExpected;


    public final boolean isPair()
    {
        return true;
    }

    public final Pair toPair()
    {
        return this;
    }


    public final boolean equal(Value other)
    {
        try
        {
            Pair otherPair = (Pair)other;

            return
                (getFirst ().equal(otherPair.getFirst ())) &&
                (getSecond().equal(otherPair.getSecond()));
        }
        catch (ClassCastException e)
        { }

        return false;
    }


    public abstract Code getCode(StaticEnvironment compilationEnv)
        throws SchemeException;


    // implementation of Compound

    public Value getCopy()
    {
        Value second = getSecond();

        if (second.isPair())
        {
            second = second.getCopy();
        }

        return create(
            getFirst(),
            second
        );
    }

    protected final Value getConstCopy()
    {
        return createConst(getFirst(), getSecond());
    }


    // implementation of Pair

    private Value _first;
    private Value _second;


    protected Pair(boolean isConst, Value first, Value second)
    {
        super(isConst);

        _first  = first;
        _second = second;
    }

    public final Value getFirst()
    {
        return _first;
    }

    public final void setFirst(Value first)
    throws ImmutableException
    {
        modify();
        _first = first;
    }

    public final Value getSecond()
    {
        return _second;
    }

    public final void setSecond(Value second)
    throws ImmutableException
    {
        modify();
        _second = second;
    }
}
