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
import io.kotest.core.spec.style.FunSpec
import io.kotest.matchers.shouldBe
import java.io.StringReader

class TestInputPort : FunSpec({
    fun portFrom(s: String) =
        InputPort.create(JvmReader(StringReader(s)))

    test("testCharIO") {
        val port = portFrom("test")

        port.isReady shouldBe true
        port.peekChar() shouldBe 't'.code
        port.readChar() shouldBe 't'.code
        port.isReady shouldBe true
        port.peekChar() shouldBe 'e'.code
        port.readChar() shouldBe 'e'.code
        port.isReady shouldBe true
        port.readChar() shouldBe 's'.code
        port.peekChar() shouldBe 't'.code
        port.isReady shouldBe true
        port.readChar() shouldBe 't'.code
        port.isReady shouldBe true
        port.peekChar() shouldBe InputPort.EOF
        port.readChar() shouldBe InputPort.EOF
        port.isReady shouldBe true
    }

    test("testWS") {
        val port = portFrom(
            " \t \n ; test# 1 2 \"\"\" ;;; 3 \n  \n ;  fkdjhgd "
        )

        ValueTraits.eq(port.read(), InputPort.EOF_VALUE) shouldBe true
    }

    test("testBool") {
        val port = portFrom(" #t #f #t#f ")

        ValueTraits.eq(port.read(), ValueTraits.TRUE) shouldBe true
        ValueTraits.eq(port.read(), ValueTraits.FALSE) shouldBe true
        ValueTraits.eq(port.read(), ValueTraits.TRUE) shouldBe true
        ValueTraits.eq(port.read(), ValueTraits.FALSE) shouldBe true
        port.readChar() shouldBe ' '.code
        port.readChar() shouldBe InputPort.EOF
    }

    test("testNumber") {
        val port = portFrom(
            "-2 -1 0 1 2"
        )

        ValueTraits.eqv(port.read(), ScmNumber.create(-2)) shouldBe true
        ValueTraits.eqv(port.read(), ScmNumber.create(-1)) shouldBe true
        ValueTraits.eqv(port.read(), ScmNumber.create(0)) shouldBe true
        ValueTraits.eqv(port.read(), ScmNumber.create(1)) shouldBe true
        ValueTraits.eqv(port.read(), ScmNumber.create(2)) shouldBe true
        port.readChar() shouldBe InputPort.EOF
    }

    test("testChar") {
        val port = portFrom(
            "#\\# #\\\\ #\\\" #\\  #\\newline #\\space #\\a"
        )

        ValueTraits.eqv(port.read(), ValueTraits.toScmChar('#')) shouldBe true
        ValueTraits.eqv(port.read(), ValueTraits.toScmChar('\\')) shouldBe true
        ValueTraits.eqv(port.read(), ValueTraits.toScmChar('"')) shouldBe true
        ValueTraits.eqv(port.read(), ValueTraits.toScmChar(' ')) shouldBe true
        ValueTraits.eqv(port.read(), ValueTraits.toScmChar('\n')) shouldBe true
        ValueTraits.eqv(port.read(), ValueTraits.toScmChar(' ')) shouldBe true
        ValueTraits.eqv(port.read(), ValueTraits.toScmChar('a')) shouldBe true
        port.readChar() shouldBe InputPort.EOF
    }

    test("testString") {
        val str1 = ""
        val str2 = " Hallo ! "
        val str3a = " \\\\ \\\" \n "
        val str3b = " \\ \" \n "

        val port = portFrom(""""$str1" "$str2" "$str3a"""")

        ValueTraits.equal(port.read(), ScmString.create(str1)) shouldBe true
        ValueTraits.equal(port.read(), ScmString.create(str2)) shouldBe true
        ValueTraits.equal(port.read(), ScmString.create(str3b)) shouldBe true
    }

    test("testList") {
        val one: Any = ScmNumber.create(1)
        val two: Any = ScmNumber.create(2)
        val three: Any = ScmNumber.create(3)

        val port = portFrom(
            "()(1 .2)(1 2 3)(1 .(2 .(3 .())))"
        )

        ValueTraits.equal(port.read(), ListFactory.create()) shouldBe true
        ValueTraits.equal(
            port.read(),
            ListFactory.createPair(one, two)
        ) shouldBe true
        ValueTraits.equal(
            port.read(), ListFactory.create(one, two, three)
        ) shouldBe true
        ValueTraits.equal(
            port.read(), ListFactory.create(one, two, three)
        ) shouldBe true
    }

    test("testVector") {
        val one: Any = ScmNumber.create(1)
        val two: Any = ScmNumber.create(2)
        val v = ScmVector.create(3, one)
        v.set(2, two)
        val port = portFrom(
            "#() #(1 1) #(1 1 2)"
        )

        ValueTraits.equal(port.read(), ScmVector.create()) shouldBe true
        ValueTraits.equal(port.read(), ScmVector.create(2, one)) shouldBe true
        ValueTraits.equal(port.read(), v) shouldBe true
    }

    test("testSymbol") {
        val test: Any = "hallo"
        val port = portFrom(
            "Hallo hallo HALLO HaLlO hAlLo + - ... ?12"
        )

        ValueTraits.eq(port.read(), test) shouldBe true
        ValueTraits.eq(port.read(), test) shouldBe true
        ValueTraits.eq(port.read(), test) shouldBe true
        ValueTraits.eq(port.read(), test) shouldBe true
        ValueTraits.eq(port.read(), test) shouldBe true
        ValueTraits.eq(port.read(), "+") shouldBe true
        ValueTraits.eq(port.read(), "-") shouldBe true
        ValueTraits.eq(port.read(), "...") shouldBe true
        ValueTraits.eq(port.read(), "?12") shouldBe true
    }

    test("testAbbrev") {
        val port = portFrom(
            "'a ' a `a ,a ,@a"
        )

        val a = "a"
        val q = "quote"
        val qq = "quasiquote"
        val uq = "unquote"
        val uqs = "unquote-splicing"

        ValueTraits.equal(port.read(), ListFactory.create(q, a)) shouldBe true
        ValueTraits.equal(port.read(), ListFactory.create(q, a)) shouldBe true
        ValueTraits.equal(port.read(), ListFactory.create(qq, a)) shouldBe true
        ValueTraits.equal(port.read(), ListFactory.create(uq, a)) shouldBe true
        ValueTraits.equal(port.read(), ListFactory.create(uqs, a)) shouldBe true
    }
})
