/*
 * Created on 02.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.values

import junit.framework.Test
import junit.framework.TestSuite
import mscheme.values.functions.AllTests

/**
 * @author sielenk
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
object AllTests {
    fun suite(): Test {
        val suite = TestSuite("Test for mscheme.values")
        //$JUnit-BEGIN$
        suite.addTestSuite(TestInputPort::class.java)
        suite.addTestSuite(OutputPortTest::class.java)
        suite.addTestSuite(ValueTraitsTest::class.java)
        suite.addTestSuite(TestList::class.java)

        //$JUnit-END$
        suite.addTest(AllTests.suite())

        return suite
    }
}
