/*
 * Created on 02.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.tests

import junit.framework.Test
import junit.framework.TestSuite

/**
 * @author sielenk
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
object AllTests {
    @JvmStatic
    fun suite(): Test {
        val suite = TestSuite("Test for mscheme.tests")
        //$JUnit-BEGIN$
        suite.addTestSuite(TestBugs::class.java)
        suite.addTestSuite(TestValue::class.java)
        suite.addTestSuite(TestMachine::class.java)
        suite.addTestSuite(TestR5RS::class.java)
        suite.addTestSuite(TestJavaInterop::class.java)
        //$JUnit-END$
        return suite
    }
}
