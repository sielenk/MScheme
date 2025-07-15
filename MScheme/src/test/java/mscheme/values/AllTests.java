/*
 * Created on 02.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.values;

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
    TestSuite suite = new TestSuite("Test for mscheme.values");
    //$JUnit-BEGIN$
    suite.addTestSuite(TestInputPort.class);
    suite.addTestSuite(OutputPortTest.class);
    suite.addTestSuite(ValueTraitsTest.class);
    suite.addTestSuite(TestList.class);
    //$JUnit-END$
    return suite;
  }
}