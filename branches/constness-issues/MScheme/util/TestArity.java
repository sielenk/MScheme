package MScheme.util;

import junit.framework.*;


public class TestArity
    extends TestCase
{
    public final static String id
        = "$Id$";

    public TestArity(String name)
    { super(name); }
    
    
    public void testExactly()
    {
        Arity arity0 = Arity.exactly(0);
        Arity arity1 = Arity.exactly(1);
        Arity arity2 = Arity.exactly(2);
    
        assert( arity0.isValid(0));
        assert(!arity0.isValid(1));
        assert(!arity0.isValid(2));
        
        assert(!arity1.isValid(0));
        assert( arity1.isValid(1));
        assert(!arity1.isValid(2));
        
        assert(!arity2.isValid(0));
        assert(!arity2.isValid(1));
        assert( arity2.isValid(2));
        assert(!arity2.isValid(3));
    }

    public void testAtLeast()
    {
        Arity arity0 = Arity.atLeast(0);
        Arity arity1 = Arity.atLeast(1);
        Arity arity2 = Arity.atLeast(2);
    
        assert( arity0.isValid(0));
        assert( arity0.isValid(1));
        assert( arity0.isValid(2));
        
        assert(!arity1.isValid(0));
        assert( arity1.isValid(1));
        assert( arity1.isValid(2));
        
        assert(!arity2.isValid(0));
        assert(!arity2.isValid(1));
        assert( arity2.isValid(2));
        assert( arity2.isValid(3));
    }

    public void testInarity()
    {
        Arity arity = Arity.inRange(2, 3);
    
        assert(!arity.isValid(0));
        assert(!arity.isValid(1));
        assert( arity.isValid(2));
        assert( arity.isValid(3));
        assert(!arity.isValid(4));      
    }
}

