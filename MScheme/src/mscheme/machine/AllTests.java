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
	public final static String CVS_ID
		= "$Id$";


	public static Test suite() {
		TestSuite suite = new TestSuite("Test for mscheme.machine");
		//$JUnit-BEGIN$
		suite.addTest(new TestSuite(MachineTest.class));
		suite.addTest(new TestSuite(StackPlainTest.class));
		suite.addTest(new TestSuite(StackListTest.class));
		//$JUnit-END$
		return suite;
	}
}
