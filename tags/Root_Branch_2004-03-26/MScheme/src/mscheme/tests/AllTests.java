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
public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite("Test for mscheme.tests");
		//$JUnit-BEGIN$
		suite.addTest(new TestSuite(TestBugs.class));
		suite.addTest(new TestSuite(TestMachine.class));
		suite.addTest(new TestSuite(TestR5RS.class));
		suite.addTest(new TestSuite(TestValue.class));
		//$JUnit-END$
		return suite;
	}
}
