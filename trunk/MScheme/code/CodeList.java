package MScheme.code;

import MScheme.machine.Machine;
import MScheme.exceptions.*;


abstract public class CodeList
{
    // static creation functions
    
    public static CodeList prepend(
        Code     head,
        CodeList tail
    )
    { return new CodeListPair(head, tail); }
    
    public static CodeList create()
    { return CodeListEmpty.getInstance(); }
    
    public static CodeList create(
        Code first
    )
    { return prepend(first, create()); }
    
    public static CodeList create(
        Code first,
        Code second
    )
    { return prepend(first, create(second)); }
    
    public static CodeList create(
        Code first,
        Code second,
        Code third
    )
    { return prepend(first, create(second, third)); }
    

    // abstract interface
        
    abstract public boolean  isEmpty();
    abstract public Code     getHead();
    abstract public CodeList getTail();
    abstract public CodeList getReversed();
}


final class CodeListPair
    extends CodeList
{
    private final Code     _head;
    private final CodeList _tail;
    
    CodeListPair(
        Code     head,
        CodeList tail
    )
    { _head = head; _tail = tail; }


    // implementation of CodeList
        
    public boolean isEmpty()
    { return false; }
    
    public Code getHead()
    { return _head; }
    
    public CodeList getTail()
    { return _tail; }

    public CodeList getReversed()
    {
        CodeList currentTail = this;
        CodeList result      = CodeList.create();
        
        while (!currentTail.isEmpty()) {
            result = CodeList.prepend(
                currentTail.getHead(),
                result
            );
            currentTail = currentTail.getTail();
        }
        
        return result;        
    }


    public Code executionStep(Machine machine)
        throws SchemeException
    { return machine.handleApplication(this); }
}


final class CodeListEmpty
    extends CodeList
{
    // construction

    private final static CodeListEmpty
        _instance = new CodeListEmpty();
    
    static CodeListEmpty getInstance()
    { return _instance; }

    // implementation of CodeList
    
    public boolean isEmpty()
    { return true; }
    
    public Code getHead()
    { throw new RuntimeException("called getHead on empty CodeList!"); }
    
    public CodeList getTail()
    { throw new RuntimeException("called getTail on empty CodeList!"); }
    
    public CodeList getReversed()
    { return this; }


    public Code executionStep(Machine machine)
    { throw new RuntimeException("can't execute the empty list"); }
}

