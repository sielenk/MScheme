package MScheme;

import junit.framework.*;


public class TestMScheme
    extends TestSuite
{
    public final static String id
        = "$Id$";

    public TestMScheme(String name)
    {
        super(name);
    
        addTestSuite(MScheme.util.TestArity.class);

        addTestSuite(MScheme.values.TestValue.class);
        addTestSuite(MScheme.values.TestList.class);
        addTestSuite(MScheme.values.TestInputPort.class);

        addTestSuite(MScheme.environment.TestEnvironment.class);

        addTestSuite(TestR5RS.class);
        addTestSuite(TestMachine.class);
    }
    
    public void testDummy()
    {
        Assert.fail("failure Test");
    }
}
