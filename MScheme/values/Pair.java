package MScheme.values;

import MScheme.machine.Machine;
import MScheme.environment.StaticEnvironment;
import MScheme.code.*;
import MScheme.exceptions.*;


public final class Pair
    extends List
{
    private Value   _first;
    private Value   _second;
    private boolean _isLiteral = false;
  

    private Pair(Value first, Value second)
    {
        _first  = first;
        _second = second;
    }

    public static Pair create(Value first, Value second)
    { return new Pair(first, second); }


    public Value setLiteral()
    { _isLiteral = true; return this; }


    // implementation of List
    
    public boolean isList()
    { return _second.isList(); }
    
    
    // specialisation/implementation of Value
    
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
    
    public Code getCode(StaticEnvironment e)
        throws SchemeException
    { return getHead().getCode(e).translateArguments(e, getTail()); }

    public CodeList getCodeList(StaticEnvironment e)
        throws SchemeException
    {
        return CodeList.prepend(
            getHead().getCode(e),
            getTail().getCodeList(e)
        );
    }
    
    // implementation of Pair
    
    public Value getFirst()
    { return _first; }
    
    public void setFirst(Value first)
        throws ImmutableException
    {
        if (_isLiteral) {
            throw new ImmutableException(this);
        }

        _first = first;
    }
    
    public Value getSecond()
    { return _second; }    

    public void setSecond(Value second)
        throws ImmutableException
    {
        if (_isLiteral) {
            throw new ImmutableException(this);
        }

        _second = second;
    }
    

    // implementation of List

    public boolean isEmpty()
    { return false; }
    
    public int getLength()
        throws ListExpectedException
    { return 1 + getTail().getLength(); }
    
    public Value getHead()
    { return getFirst(); }
    
    public List getTail()
        throws ListExpectedException
    { return getSecond().toList(); }
    
    public List getReversed()
        throws ListExpectedException
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

