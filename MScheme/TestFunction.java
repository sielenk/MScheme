package MScheme;

import junit.framework.*;
import MScheme.values.*;


public class TestFunction
    extends TestCase
{
    public TestFunction(String name)
    { super(name); }
    
    
    public void testCallCC()
        throws Exception
    {
        ValueFactory.createFunction("CallCC");
    }
}

