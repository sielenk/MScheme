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
 * Created on 03.01.2004
 */
package de.masitec.mscheme.values

import de.masitec.mscheme.util.createReaderWriterPipe
import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.shouldBe

/**
 * @author sielenk
 */
class OutputPortTest : FunSpec({
    fun checkReadWrite(expected: Any?) {
        val (reader, writer) = createReaderWriterPipe()

        writer.use {
            val outputPort = OutputPort.create(it)

            outputPort.write(expected)
            outputPort.close()
        }

        val actual: Any?

        reader.use {
            val inputPort = InputPort.create(reader)

            actual = inputPort.read()
            inputPort.close()
        }

        ValueTraits.equal(expected, actual) shouldBe true
    }

    test("testReadWriteBoolean") {
        checkReadWrite(ValueTraits.TRUE)
        checkReadWrite(ValueTraits.FALSE)
    }

    test("testReadWriteChar") {
        checkReadWrite(ValueTraits.toScmChar('a'))
        checkReadWrite(ValueTraits.toScmChar('\n'))
        checkReadWrite(ValueTraits.toScmChar(' '))
    }

    test("testReadWriteNumber") {
        checkReadWrite(ValueTraits.toScmNumber(-1))
        checkReadWrite(ValueTraits.toScmNumber(0))
        checkReadWrite(ValueTraits.toScmNumber(12))
    }

    test("testReadWriteList") {
        checkReadWrite(ListFactory.create())
        checkReadWrite(ListFactory.create(ValueTraits.TRUE))
        checkReadWrite(
            ListFactory.create(
                ValueTraits.toScmChar('b'),
                ValueTraits.toScmChar('\n')
            )
        )
    }

    test("testReadWriteVector") {
        checkReadWrite(ScmVector.create())
        checkReadWrite(
            ScmVector.create(
                arrayOf(
                    ValueTraits.toScmChar('b'),
                    ValueTraits.toScmChar('\n')
                )
            )
        )
    }

    test("testReadWriteString") {
        checkReadWrite(ScmString.create("Hallo World"))
        checkReadWrite(ScmString.create("And now the bad bits: \n \" \\ öäü"))
    }
})
