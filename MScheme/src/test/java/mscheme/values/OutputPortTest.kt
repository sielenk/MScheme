/*
 * Created on 03.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.values

import junit.framework.TestCase
import mscheme.values.ValueTraits.equal
import java.io.PipedReader
import java.io.PipedWriter

/**
 * @author sielenk
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
class OutputPortTest(name: String?) : TestCase(name) {
    fun testReadWriteBoolean() {
        checkReadWrite(ValueTraits.TRUE)
        checkReadWrite(ValueTraits.FALSE)
    }

    fun testReadWriteChar() {
        checkReadWrite(ValueTraits.toScmChar('a'))
        checkReadWrite(ValueTraits.toScmChar('\n'))
        checkReadWrite(ValueTraits.toScmChar(' '))
    }

    fun testReadWriteNumber() {
        checkReadWrite(ValueTraits.toScmNumber(-1))
        checkReadWrite(ValueTraits.toScmNumber(0))
        checkReadWrite(ValueTraits.toScmNumber(12))
    }

    fun testReadWriteList() {
        checkReadWrite(ListFactory.create())
        checkReadWrite(ListFactory.create(ValueTraits.TRUE))
        checkReadWrite(
            ListFactory.create(
                ValueTraits.toScmChar('b'),
                ValueTraits.toScmChar('\n')
            )
        )
    }

    fun testReadWriteVector() {
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

    fun testReadWriteString() {
        checkReadWrite(ScmString.create("Hallo World"))
        checkReadWrite(ScmString.create("And now the bad bits: \n \" \\ öäü"))
    }

    companion object {
        private fun checkReadWrite(expected: Any?) {
            val inPipe = PipedReader()

            run {
                val outPipe = PipedWriter(inPipe)
                val out = OutputPort.create(outPipe)
                out.write(expected)
                out.close()
                outPipe.close()
            }

            val actual = InputPort.create(inPipe).read()

            assertTrue(equal(expected, actual))
        }
    }
}
