/* Some junit tests for the parser in InputPort.
   Copyright (C) 2001  Marvin H. Sielenkemper

This file is part of MScheme.

MScheme is free software; you can redistribute it and/or modify 
it under the terms of the GNU General Public License as published by 
the Free Software Foundation; either version 2 of the License, 
or (at your option) any later version. 

MScheme is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details. 

You should have received a copy of the GNU General Public License
along with MScheme; see the file COPYING. If not, write to 
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA  02111-1307, USA. */
package mscheme.values

import junit.framework.TestCase
import mscheme.values.InputPort.Companion.EOF_VALUE
import mscheme.values.ListFactory.create
import mscheme.values.ListFactory.createPair
import mscheme.values.ValueTraits.eq
import mscheme.values.ValueTraits.equal
import mscheme.values.ValueTraits.eqv
import java.io.StringReader

class TestInputPort
    (name: String?) : TestCase(name) {
    @Throws(Exception::class)
    fun testCharIO() {
        val source = StringReader("test")
        val `in` = InputPort.Companion.create(source)

        assertTrue(`in`.isReady)
        TestCase.assertEquals('t'.code, `in`.peekChar())
        TestCase.assertEquals('t'.code, `in`.readChar())
        assertTrue(`in`.isReady)
        TestCase.assertEquals('e'.code, `in`.peekChar())
        TestCase.assertEquals('e'.code, `in`.readChar())
        assertTrue(`in`.isReady)
        TestCase.assertEquals('s'.code, `in`.readChar())
        TestCase.assertEquals('t'.code, `in`.peekChar())
        assertTrue(`in`.isReady)
        TestCase.assertEquals('t'.code, `in`.readChar())
        assertTrue(`in`.isReady)
        TestCase.assertEquals(`in`.peekChar(), InputPort.EOF)
        TestCase.assertEquals(`in`.readChar(), InputPort.EOF)
        assertTrue(`in`.isReady)
    }

    @Throws(Exception::class)
    fun testWS() {
        val source = StringReader(
            " \t \n ; test# 1 2 \"\"\" ;;; 3 \n  \n ;  fkdjhgd "
        )
        val `in` = InputPort.Companion.create(source)

        assertTrue(eq(`in`.read(), EOF_VALUE))
    }

    @Throws(Exception::class)
    fun testBool() {
        val source = StringReader(" #t #f #t#f ")
        val `in` = InputPort.Companion.create(source)

        assertTrue(eq(`in`.read(), ValueTraits.TRUE))
        assertTrue(eq(`in`.read(), ValueTraits.FALSE))
        assertTrue(eq(`in`.read(), ValueTraits.TRUE))
        assertTrue(eq(`in`.read(), ValueTraits.FALSE))
        TestCase.assertEquals(' '.code, `in`.readChar())
        TestCase.assertEquals(`in`.readChar(), InputPort.EOF)
    }

    @Throws(Exception::class)
    fun testNumber() {
        val source = StringReader(
            "-2 -1 0 1 2"
        )
        val `in` = InputPort.Companion.create(source)

        assertTrue(eqv(`in`.read(), ScmNumber.Companion.create(-2)))
        assertTrue(eqv(`in`.read(), ScmNumber.Companion.create(-1)))
        assertTrue(eqv(`in`.read(), ScmNumber.Companion.create(0)))
        assertTrue(eqv(`in`.read(), ScmNumber.Companion.create(1)))
        assertTrue(eqv(`in`.read(), ScmNumber.Companion.create(2)))
        TestCase.assertEquals(`in`.readChar(), InputPort.EOF)
    }

    @Throws(Exception::class)
    fun testChar() {
        val source = StringReader(
            "#\\# #\\\\ #\\\" #\\  #\\newline #\\space #\\a"
        )
        val `in` = InputPort.Companion.create(source)

        assertTrue(eqv(`in`.read(), ValueTraits.toScmChar('#')))
        assertTrue(eqv(`in`.read(), ValueTraits.toScmChar('\\')))
        assertTrue(eqv(`in`.read(), ValueTraits.toScmChar('"')))
        assertTrue(eqv(`in`.read(), ValueTraits.toScmChar(' ')))
        assertTrue(eqv(`in`.read(), ValueTraits.toScmChar('\n')))
        assertTrue(eqv(`in`.read(), ValueTraits.toScmChar(' ')))
        assertTrue(eqv(`in`.read(), ValueTraits.toScmChar('a')))
        TestCase.assertEquals(`in`.readChar(), InputPort.EOF)
    }

    @Throws(Exception::class)
    fun testString() {
        val str1 = ""
        val str2 = " Hallo ! "
        val str3a = " \\\\ \\\" \n "
        val str3b = " \\ \" \n "

        val source = StringReader(
            ('"'.toString() + str1 + '"' + ' '
                    + '"' + str2 + '"' + ' '
                    + '"' + str3a + '"')
        )
        val `in` = InputPort.Companion.create(source)

        assertTrue(equal(`in`.read(), ScmString.Companion.create(str1)))
        assertTrue(equal(`in`.read(), ScmString.Companion.create(str2)))
        assertTrue(equal(`in`.read(), ScmString.Companion.create(str3b)))
    }

    @Throws(Exception::class)
    fun testList() {
        val one: Any = ScmNumber.Companion.create(1)
        val two: Any = ScmNumber.Companion.create(2)
        val three: Any = ScmNumber.Companion.create(3)

        val source = StringReader(
            "()(1 .2)(1 2 3)(1 .(2 .(3 .())))"
        )
        val `in` = InputPort.Companion.create(source)

        assertTrue(equal(`in`.read(), ListFactory.create()))
        assertTrue(equal(`in`.read(), createPair(one, two)))
        assertTrue(
            equal(`in`.read(), create(one, two, three))
        )
        assertTrue(
            equal(`in`.read(), create(one, two, three))
        )
    }

    @Throws(Exception::class)
    fun testVector() {
        val one: Any = ScmNumber.Companion.create(1)
        val two: Any = ScmNumber.Companion.create(2)
        val v = ScmVector.Companion.create(3, one)
        v.set(2, two)
        val source = StringReader(
            "#() #(1 1) #(1 1 2)"
        )
        val `in` = InputPort.Companion.create(source)

        assertTrue(equal(`in`.read(), ScmVector.Companion.create()))
        assertTrue(equal(`in`.read(), ScmVector.Companion.create(2, one)))
        assertTrue(equal(`in`.read(), v))
    }

    @Throws(Exception::class)
    fun testSymbol() {
        val test: Any = "hallo"
        val source = StringReader(
            "Hallo hallo HALLO HaLlO hAlLo + - ... ?12"
        )
        val `in` = InputPort.Companion.create(source)

        assertTrue(eq(`in`.read(), test))
        assertTrue(eq(`in`.read(), test))
        assertTrue(eq(`in`.read(), test))
        assertTrue(eq(`in`.read(), test))
        assertTrue(eq(`in`.read(), test))
        assertTrue(eq(`in`.read(), "+"))
        assertTrue(eq(`in`.read(), "-"))
        assertTrue(eq(`in`.read(), "..."))
        assertTrue(eq(`in`.read(), "?12"))
    }

    @Throws(Exception::class)
    fun testAbbrev() {
        val source = StringReader(
            "'a ' a `a ,a ,@a"
        )
        val `in` = InputPort.Companion.create(source)

        val a = "a"
        val q = "quote"
        val qq = "quasiquote"
        val uq = "unquote"
        val uqs = "unquote-splicing"

        assertTrue(equal(`in`.read(), create(q, a)))
        assertTrue(equal(`in`.read(), create(q, a)))
        assertTrue(equal(`in`.read(), create(qq, a)))
        assertTrue(equal(`in`.read(), create(uq, a)))
        assertTrue(equal(`in`.read(), create(uqs, a)))
    }
}
