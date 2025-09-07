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
package de.masitec.mscheme

import junit.framework.Test
import junit.framework.TestSuite
import junit.textui.TestRunner
import de.masitec.mscheme.environment.TestEnvironment
import de.masitec.mscheme.tests.AllTests
import de.masitec.mscheme.machine.AllTests as MachineAllTests
import de.masitec.mscheme.util.TestArity
import de.masitec.mscheme.values.AllTests as ValuesAllTests

object TestMScheme {
    fun suite(): Test {
        val suite = TestSuite("All MScheme Tests")

        suite.addTestSuite(TestArity::class.java)
        suite.addTestSuite(TestEnvironment::class.java)

        suite.addTest(MachineAllTests.suite())
        suite.addTest(ValuesAllTests.suite())
        suite.addTest(AllTests.suite())

        return suite
    }

    @JvmStatic
    fun main(args: Array<String>) {
        TestRunner.run(
            suite()
        )
    }
}
