package MScheme.values;

import MScheme.exceptions.*;


public class TestList
    extends junit.framework.TestCase
{
    Value firstElement;
    Value secondElement;
    Value lastElement;
    List  emptyList;
    List  occupiedList;
    int   occupiedListLength;
        
    public TestList(String name)
    { super(name); }
    
    protected void setUp()
    {
        emptyList     = ValueFactory.createList();
    
        firstElement  = ValueFactory.createSymbol("x");
        secondElement = ValueFactory.createSymbol("y");
        lastElement   = secondElement;
        occupiedList  = ValueFactory.createList(
                firstElement,
                secondElement
            );
        occupiedListLength = 2;
    }
    
    protected void tearDown()
    {
        firstElement  = null;
        secondElement = null;
        emptyList     = null;
        occupiedList  = null;
    }
    

    public void testTestValues()
    {
        assert(
            occupiedListLength >= 2
        );
        
        assert(
            firstElement != secondElement
        );

        assert(
            firstElement != lastElement
        );
    }
    
    public void testEmptyIsUnique()
        throws Exception
    {
        assert(
            "empty isn't unique",
            emptyList == ValueFactory.createList()
        );
    }
    
    public void testOccupiedList()
        throws Exception
    {
        assert(
            "occupied list equals (==) empty list",
            occupiedList != emptyList
        );

        assert(
            "toPair returned somethig wrong",
            occupiedList.toPair().getFirst() == occupiedList.getHead()
        );
    }
    
    public void testIsEmpty()
    {
        assert(
            emptyList.isEmpty()
        );
        
        assert(
            !occupiedList.isEmpty()
        );
    }
    
    public void testGetLength()
        throws Exception
    {
        assert(
            emptyList.getLength() == 0
        );
        
        assert(
            occupiedList.getLength() == occupiedListLength
        );
    }
    
    public void testGetHead()
        throws Exception
    {
        try {
            Value dummy = emptyList.getHead();
            fail("PairExpectedException expected");
        }
        catch (PairExpectedException e) { }
        
        assert(
            "getHead failed",
            occupiedList.getHead() == firstElement
        );
    }    

    public void testGetTail()
        throws Exception
    {
        try {
            assert(emptyList.getTail() != null);
            fail("PairExpectedException expected");
        }
        catch (PairExpectedException e) { }
        
        assert(
            "getTail failed",
            occupiedList.getTail().getHead() == secondElement
        );
    }
    
    public void testGetReversed()
        throws Exception
    {
        assert(
            "failed on emptyList",
            emptyList.getReversed() == emptyList
        );
        
        assert(
            "length mismatch ",
            occupiedList.getReversed().getLength() == occupiedListLength
        );

        assert(
            "previous last isn't first now",
            occupiedList.getReversed().getHead() == lastElement
        );
    }
    
    public void testGetCodeList()
    {
        // ...
    }
}

