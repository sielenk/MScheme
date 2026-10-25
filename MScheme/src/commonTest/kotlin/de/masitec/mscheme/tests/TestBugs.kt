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
package de.masitec.mscheme.tests

import de.masitec.mscheme.exceptions.CompileError
import io.kotest.assertions.throwables.shouldThrow
import io.kotest.core.spec.style.FunSpec

class TestBugs : FunSpec({
    val mh = MachineHolder(this)

    test("test_2002_19_03") {
        // This failed, because set! didn't use delayed
        // references, MHS 2002-19-03
        mh.check(
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

    test("test_2002_04_09") {
        // It is now illegal to internally redefine a symbol.
        shouldThrow<CompileError> {
            mh.eval("(let ((x 1)) (begin (define x 2)) x)")
        }
    }

    test("test_2002_04_15a") {
        // Internal definitions are not allowed after the
        // first expression.

        shouldThrow<CompileError> {
            mh.eval("(let () 1 (define x 2) x)")
        }

        shouldThrow<CompileError> {
            mh.eval("(let () 'a (define x 2) x)")
        }

        shouldThrow<CompileError> {
            mh.eval("(let ((a 1)) a (define x 2) x)")
        }

        shouldThrow<CompileError> {
            mh.eval("(let () (if 1 1 1) (define x 2) x)")
        }
    }

    test("test_2002_04_15ba") {
        // no nested definitions
        shouldThrow<CompileError> {
            mh.eval("(define x (define y 3))")
        }
    }

    test("test_2002_04_15bb") {
        // no nested definitions
        shouldThrow<CompileError> {
            mh.eval("(lambda () (cons (define a 1) 2))")
        }
    }

    test("test_2002_04_15c") {
        shouldThrow<CompileError> {
            mh.eval(
                """
                    (define (f)
                      (define y 2)
                      (define g (+ y 1))
                      g
                      y))
                """.trimIndent()
            )
        }

        shouldThrow<CompileError> {
            mh.eval(
                """
                    (define (f)
                      (define g (+ y 1))
                      (define y 2)
                      g
                      y))
                """.trimIndent()
            )
        }

        shouldThrow<CompileError> {
            mh.eval(
                """
                    (define (f)
                      (define y 2)
                      (define g (set! y 1))
                      g
                      y))
                """.trimIndent()
            )
        }
    }

    test("todo01") {
        shouldThrow<CompileError> {
            mh.eval("(cons (define a 1) 2)")
        }
    }

    test("test_define_contexts") {
        // definitions are only allowed at the top level and
        // at the start of a body, never inside of an expression
        shouldThrow<CompileError> {
            mh.eval("(if #t (define a 1))")
        }
        shouldThrow<CompileError> {
            mh.eval("(cons (begin (define a 1)) 2)")
        }
        shouldThrow<CompileError> {
            mh.eval("(define a (begin (define b 1) b))")
        }

        // a top level begin may mix definitions and expressions
        mh.check(
            "(begin (define a 1) (set! a (+ a 1)) (define b a) b)",
            "2"
        )

        // a begin in a body is spliced into it
        mh.check(
            "(let () (begin (define a 1) (define b 2)) (+ a b))",
            "3"
        )
        shouldThrow<CompileError> {
            mh.eval("(let () (begin (define a 1) a) (define b 2) b)")
        }

        // eval compiles at the top level of the given environment
        mh.eval("(eval '(define c 3) machine-environment)")
        mh.check("c", "3")
    }

    test("test_define_by_macro") {
        // a macro expanding to a definition counts as one
        mh.eval(
            """
                (define-syntax def1
                  (lambda (def-env use-env name)
                    (cons use-env (list 'define name 1))))
            """.trimIndent()
        )
        mh.check(
            "(let () (def1 x) (def1 y) (+ x y))",
            "2"
        )
    }
})
