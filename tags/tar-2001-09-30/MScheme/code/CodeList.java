package MScheme.code;

import MScheme.Code;

import MScheme.exceptions.*;


public abstract class CodeList
{
    public final static String id
        = "$Id$";


    protected CodeList()
    { }

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

    public static CodeList create(
        Code first,
        Code second,
        Code third,
        Code fourth
    )
    { return prepend(first, create(second, third, fourth)); }


    public String toString()
    {
        StringBuffer buffer = new StringBuffer();
        
        for (CodeList current = this;;) {
            buffer.append(current.getHead().toString());
            current = current.getTail();
            if (current.isEmpty()) {
                break;
            }
            buffer.append(' ');
        }

        return buffer.toString();
    }

    // abstract interface

    public abstract boolean  isEmpty();
    public abstract Code     getHead();
    public abstract CodeList getTail();
    public abstract CodeList getReversed();
}


final class CodeListPair
    extends CodeList
{
    public final static String id
        = "$Id$";


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
}


final class CodeListEmpty
    extends CodeList
{
    public final static String id
        = "$Id$";


    // construction

    private CodeListEmpty()
    { }

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
}
