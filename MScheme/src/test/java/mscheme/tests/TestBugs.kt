/* 
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
package mscheme.tests

import mscheme.exceptions.CompileError
import mscheme.exceptions.SchemeException

class TestBugs
    (name: String?) : TestSchemeBase(name) {
    @Throws(SchemeException::class, InterruptedException::class)
    fun test_2002_19_03() {
        // This failed, because set! didn't use delayed
        // references, MHS 2002-19-03
        check(
            """
            (begin
              (define (f)
                (define (g)
                  (set! x 1)
                  x)
                (define x 2)
                (g)
                x)
              (f))
              """.trimIndent(),
            "1"
        )
    }

    @Throws(SchemeException::class, InterruptedException::class)
    fun test_2002_04_09() {
        // It is now illegal to internally redefine a symbol.
        try {
            eval("(let ((x 1)) (begin (define x 2)) x)")
            fail()
        } catch (e: CompileError) {
        }
    }

    @Throws(SchemeException::class, InterruptedException::class)
    fun test_2002_04_15a() {
        // Internal definitions are not allowed after the
        // first expression.

        try {
            eval("(let () 1 (define x 2) x)")
            fail()
        } catch (e: CompileError) {
        }

        try {
            eval("(let () 'a (define x 2) x)")
            fail()
        } catch (e: CompileError) {
        }

        try {
            eval("(let ((a 1)) a (define x 2) x)")
            fail()
        } catch (e: CompileError) {
        }

        try {
            eval("(let () (if 1 1 1) (define x 2) x)")
            fail()
        } catch (e: CompileError) {
        }
    }

    @Throws(SchemeException::class, InterruptedException::class)
    fun test_2002_04_15ba() {
        // no nested definitions
        try {
            eval("(define x (define y 3))")
            fail()
        } catch (e: CompileError) {
        }
    }

    @Throws(SchemeException::class, InterruptedException::class)
    fun test_2002_04_15bb() {
        // no nested definitions
        try {
            eval("(lambda () (cons (define a 1) 2))")
            fail()
        } catch (e: CompileError) {
        }
    }

    @Throws(SchemeException::class, InterruptedException::class)
    fun test_2002_04_15c() {
        try {
            eval(
                """
          (define (f)
            (define y 2)
            (define g (+ y 1))
            g
            y))
            """.trimIndent()
            )
            fail()
        } catch (e: CompileError) {
        }

        try {
            eval(
                """
          (define (f)
            (define g (+ y 1))
            (define y 2)
            g
            y))
            """.trimIndent()
            )
            fail()
        } catch (e: CompileError) {
        }

        try {
            eval(
                """
          (define (f)
            (define y 2)
            (define g (set! y 1))
            g
            y))
            """.trimIndent()
            )
            fail()
        } catch (e: CompileError) {
        }
    }

    @Throws(SchemeException::class, InterruptedException::class)
    fun xtest_todo01() {
        try {
            eval("(cons (define a 1) 2)")
            fail()
        } catch (e: CompileError) {
        }
    }
}
