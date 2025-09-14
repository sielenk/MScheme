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
package de.masitec.mscheme.values

import de.masitec.mscheme.util.JvmReader
import junit.framework.TestCase
import java.io.StringReader

class TestInputPort(name: String?) : TestCase(name) {
    fun testCharIO() {
        val source = StringReader("test")
        val `in` = InputPort.create(JvmReader(source))

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

    fun testWS() {
        val source = StringReader(
            " \t \n ; test# 1 2 \"\"\" ;;; 3 \n  \n ;  fkdjhgd "
        )
        val `in` = InputPort.create(JvmReader(source))

        assertTrue(ValueTraits.eq(`in`.read(), InputPort.EOF_VALUE))
    }

    fun testBool() {
        val source = StringReader(" #t #f #t#f ")
        val `in` = InputPort.create(JvmReader(source))

        assertTrue(ValueTraits.eq(`in`.read(), ValueTraits.TRUE))
        assertTrue(ValueTraits.eq(`in`.read(), ValueTraits.FALSE))
        assertTrue(ValueTraits.eq(`in`.read(), ValueTraits.TRUE))
        assertTrue(ValueTraits.eq(`in`.read(), ValueTraits.FALSE))
        TestCase.assertEquals(' '.code, `in`.readChar())
        TestCase.assertEquals(`in`.readChar(), InputPort.EOF)
    }

    fun testNumber() {
        val source = StringReader(
            "-2 -1 0 1 2"
        )
        val `in` = InputPort.create(JvmReader(source))

        assertTrue(ValueTraits.eqv(`in`.read(), ScmNumber.create(-2)))
        assertTrue(ValueTraits.eqv(`in`.read(), ScmNumber.create(-1)))
        assertTrue(ValueTraits.eqv(`in`.read(), ScmNumber.create(0)))
        assertTrue(ValueTraits.eqv(`in`.read(), ScmNumber.create(1)))
        assertTrue(ValueTraits.eqv(`in`.read(), ScmNumber.create(2)))
        TestCase.assertEquals(`in`.readChar(), InputPort.EOF)
    }

    fun testChar() {
        val source = StringReader(
            "#\\# #\\\\ #\\\" #\\  #\\newline #\\space #\\a"
        )
        val `in` = InputPort.create(JvmReader(source))

        assertTrue(ValueTraits.eqv(`in`.read(), ValueTraits.toScmChar('#')))
        assertTrue(ValueTraits.eqv(`in`.read(), ValueTraits.toScmChar('\\')))
        assertTrue(ValueTraits.eqv(`in`.read(), ValueTraits.toScmChar('"')))
        assertTrue(ValueTraits.eqv(`in`.read(), ValueTraits.toScmChar(' ')))
        assertTrue(ValueTraits.eqv(`in`.read(), ValueTraits.toScmChar('\n')))
        assertTrue(ValueTraits.eqv(`in`.read(), ValueTraits.toScmChar(' ')))
        assertTrue(ValueTraits.eqv(`in`.read(), ValueTraits.toScmChar('a')))
        TestCase.assertEquals(`in`.readChar(), InputPort.EOF)
    }

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
        val `in` = InputPort.create(JvmReader(source))

        assertTrue(ValueTraits.equal(`in`.read(), ScmString.create(str1)))
        assertTrue(ValueTraits.equal(`in`.read(), ScmString.create(str2)))
        assertTrue(ValueTraits.equal(`in`.read(), ScmString.create(str3b)))
    }

    fun testList() {
        val one: Any = ScmNumber.create(1)
        val two: Any = ScmNumber.create(2)
        val three: Any = ScmNumber.create(3)

        val source = StringReader(
            "()(1 .2)(1 2 3)(1 .(2 .(3 .())))"
        )
        val `in` = InputPort.create(JvmReader(source))

        assertTrue(ValueTraits.equal(`in`.read(), ListFactory.create()))
        assertTrue(
            ValueTraits.equal(
                `in`.read(),
                ListFactory.createPair(one, two)
            )
        )
        assertTrue(
            ValueTraits.equal(`in`.read(), ListFactory.create(one, two, three))
        )
        assertTrue(
            ValueTraits.equal(`in`.read(), ListFactory.create(one, two, three))
        )
    }

    fun testVector() {
        val one: Any = ScmNumber.create(1)
        val two: Any = ScmNumber.create(2)
        val v = ScmVector.create(3, one)
        v.set(2, two)
        val source = StringReader(
            "#() #(1 1) #(1 1 2)"
        )
        val `in` = InputPort.create(JvmReader(source))

        assertTrue(ValueTraits.equal(`in`.read(), ScmVector.create()))
        assertTrue(ValueTraits.equal(`in`.read(), ScmVector.create(2, one)))
        assertTrue(ValueTraits.equal(`in`.read(), v))
    }

    fun testSymbol() {
        val test: Any = "hallo"
        val source = StringReader(
            "Hallo hallo HALLO HaLlO hAlLo + - ... ?12"
        )
        val `in` = InputPort.create(JvmReader(source))

        assertTrue(ValueTraits.eq(`in`.read(), test))
        assertTrue(ValueTraits.eq(`in`.read(), test))
        assertTrue(ValueTraits.eq(`in`.read(), test))
        assertTrue(ValueTraits.eq(`in`.read(), test))
        assertTrue(ValueTraits.eq(`in`.read(), test))
        assertTrue(ValueTraits.eq(`in`.read(), "+"))
        assertTrue(ValueTraits.eq(`in`.read(), "-"))
        assertTrue(ValueTraits.eq(`in`.read(), "..."))
        assertTrue(ValueTraits.eq(`in`.read(), "?12"))
    }

    fun testAbbrev() {
        val source = StringReader(
            "'a ' a `a ,a ,@a"
        )
        val `in` = InputPort.create(JvmReader(source))

        val a = "a"
        val q = "quote"
        val qq = "quasiquote"
        val uq = "unquote"
        val uqs = "unquote-splicing"

        assertTrue(ValueTraits.equal(`in`.read(), ListFactory.create(q, a)))
        assertTrue(ValueTraits.equal(`in`.read(), ListFactory.create(q, a)))
        assertTrue(ValueTraits.equal(`in`.read(), ListFactory.create(qq, a)))
        assertTrue(ValueTraits.equal(`in`.read(), ListFactory.create(uq, a)))
        assertTrue(ValueTraits.equal(`in`.read(), ListFactory.create(uqs, a)))
    }
}
