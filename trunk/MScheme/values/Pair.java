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
  

    Pair(Value first, Value second)
    {
        _first  = first;
        _second = second;
    }


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
    { return getHead().getCode(e).transform(e, getTail()); }

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
    { _first = first; }
    
    public Value getSecond()
    { return _second; }    

    public void setSecond(Value second)
    { _second = second; }
    

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

