package MScheme.values;

import java.io.Writer;
import java.io.IOException;

import MScheme.Value;
import MScheme.Code;

import MScheme.machine.Machine;
import MScheme.environment.StaticEnvironment;
import MScheme.code.*;

import MScheme.exceptions.*;


public final class Pair
    extends    Compound
    implements List
{
    private Value   _first;
    private Value   _second;


    private Pair(Value first, Value second)
    {
        _first  = first;
        _second = second;
    }

    public static Pair create(Value first, Value second)
    { return new Pair(first, second); }


    // implementation of List
    
    public boolean isList()
    { return _second.isList(); }

    public Value toValue()
    { return this; }

    // specialisation/implementation of Value
    
    private void put(Writer destination, boolean doDisplay)
        throws IOException
    {
        boolean first   = true;
        Value   current = this;
        
        destination.write('(');
        
        try {
            while (current.isPair()) {
                Pair pair = current.toPair();
        
                if (first) {
                    first = false;
                } else {
                    destination.write(' ');
                }
            
                if (doDisplay) {
                    pair.getFirst().display(destination);
                } else {
                    pair.getFirst().write(destination);
                }
                current = pair.getSecond();
            }
        }
        catch (PairExpected e) {
            throw new RuntimeException(
                "unexpected PairExpected"
            );
        }
        
        if (!current.isList()) {
            destination.write(" . ");
            if (doDisplay) {
                current.display(destination);
            } else {
                current.write(destination);
            }
        }
        
        destination.write(')');
    }

    public void write(Writer destination)
        throws IOException
    { put(destination, false); }

    public void display(Writer destination)
        throws IOException
    { put(destination, true); }


    public final List toList()
    { return this; }

    public boolean isPair()
    { return true; }
    
    public Pair toPair()
    { return this; }

    public boolean equal(Value other)
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
    
    public Code getCode(StaticEnvironment env)
        throws CompileError, TypeError
    { return getHead().getToken(env).translate(env, getTail()); }

    public CodeList getCodeList(StaticEnvironment env)
        throws CompileError, TypeError
    {
        return CodeList.prepend(
            getHead().getCode    (env),
            getTail().getCodeList(env)
        );
    }
    

    // implementation of Pair
    
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
    

    // implementation of List

    public boolean isEmpty()
    { return false; }
    
    public int safeGetLength()
    {
        Value current = getSecond();
        int   result  = 1;
        while (current instanceof Pair) {
            current = ((Pair)current).getSecond();
            ++result;
        }
        return current.isList() ? result : -1;
    }

    public int getLength()
        throws ListExpected
    {
        int result = safeGetLength();

        if (result < 0) {
            throw new ListExpected(this);
        } else {
            return result;
        }
    }
    
    public Value getHead()
    { return getFirst(); }
    
    public List getTail()
        throws ListExpected
    { return getSecond().toList(); }

    public List getReversed()
        throws ListExpected
    {
        List currentTail = toList();
        List result      = ValueFactory.createList();
        
        while (!currentTail.isEmpty()) {
            result = ValueFactory.prepend(
                currentTail.getHead(),
                result
            );
            currentTail = currentTail.getTail();
        }
        
        return result;        
    }    
}
