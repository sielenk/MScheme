/*
 * Created on 02.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.machine

import junit.framework.Test
import junit.framework.TestSuite
import mscheme.machine.stack.StackListTest
import mscheme.machine.stack.StackPlainTest

/**
 * @author sielenk
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
object AllTests {
    @JvmStatic
    fun suite(): Test {
        val suite = TestSuite("Test for mscheme.machine")
        //$JUnit-BEGIN$
        suite.addTestSuite(StackListTest::class.java)
        suite.addTestSuite(StackPlainTest::class.java)
        //$JUnit-END$
        return suite
    }
}
