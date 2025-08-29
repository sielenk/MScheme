/*
 * Created on 02.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.values

import junit.framework.TestCase
import mscheme.values.ValueTraits.isTrue
import java.lang.Boolean
import kotlin.Any
import kotlin.String

/**
 * @author sielenk
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
class ValueTraitsTest
/**
 * Constructor for ValueTraitsTest.
 */
    (name: String?) : TestCase(name) {
    fun testIsTrue() {
        assertTrue(isTrue(Boolean.TRUE))
        assertFalse(isTrue(Boolean.FALSE))

        assertTrue(isTrue(ValueTraits.TRUE))
        assertFalse(isTrue(ValueTraits.FALSE))

        assertTrue(isTrue(Any()))
    }
}
