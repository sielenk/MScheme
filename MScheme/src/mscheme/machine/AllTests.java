/*
 * Created on 02.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.machine;

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
    public final static String CVS_ID = "$Id$";

    public static Test suite()
    {
        TestSuite suite = new TestSuite("Test for mscheme.machine");
        //$JUnit-BEGIN$
        suite.addTestSuite(StackListTest.class);
        suite.addTestSuite(StackPlainTest.class);
        suite.addTestSuite(MachineTest.class);
        //$JUnit-END$
        return suite;
    }
}