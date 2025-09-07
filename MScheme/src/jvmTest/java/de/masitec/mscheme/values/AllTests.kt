/*
 * Copyright (C) 2025  Marvin H. Sielenkemper
 *
 * This file is part of MScheme.
 *
 * MScheme is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License,
 * or (at your option) any later version.
 *
 * MScheme is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with MScheme; see the file COPYING. If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA  02111-1307, USA.
 */

/*
 * Created on 02.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package de.masitec.mscheme.values

import junit.framework.Test
import junit.framework.TestSuite
import de.masitec.mscheme.values.functions.AllTests

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
