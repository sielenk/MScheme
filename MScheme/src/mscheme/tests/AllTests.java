/*
 * Created on 02.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.tests;

import junit.framework.Test;
import junit.framework.TestSuite;

/**
 * @author sielenk
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class AllTests
{

    public static Test suite()
    {
        TestSuite suite = new TestSuite("Test for mscheme.tests");
        //$JUnit-BEGIN$
        suite.addTestSuite(TestBugs.class);
        suite.addTestSuite(TestValue.class);
        suite.addTestSuite(TestMachine.class);
        suite.addTestSuite(TestR5RS.class);
        //$JUnit-END$
        return suite;
    }
}