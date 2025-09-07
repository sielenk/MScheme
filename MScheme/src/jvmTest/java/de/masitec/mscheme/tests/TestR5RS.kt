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
import de.masitec.mscheme.exceptions.ImmutableException
import de.masitec.mscheme.exceptions.SchemeException
import de.masitec.mscheme.exceptions.SyntaxArityError
import de.masitec.mscheme.values.Function
import de.masitec.mscheme.values.ScmString
import de.masitec.mscheme.values.ScmVector
import de.masitec.mscheme.values.ValueTraits.toMutablePair

class TestR5RS
    (name: String?) : TestSchemeBase(name) {
    // 4 Expressions
    // 4.1 Primitive expression types
    /** 4.1.1 Variable references */
    fun test4_1_1() {
        eval("(define x 28)")
        check("x", "28")
    }

    /** 4.1.2 Literal expressions */
    fun test4_1_2() {
        check("(quote a)", "a")
        check("(quote #(a b c))", "#(a b c)")
        check("(quote (+ 1 2))", "(+ 1 2)")

        check("'a", "a")
        check("'#(a b c)", "#(a b c)")
        check("'()", "()")
        check("'(+ 1 2)", "(+ 1 2)")
        check("'(quote a)", "(quote a)")
        check("''a", "(quote a)")

        check("'\"abc\"", "\"abc\"")
        check("\"abc\"", "\"abc\"")
        check("'145932", "145932")
        check("145932", "145932")
        check("'#t", "#t")
        check("#t", "#t")

        try {
            toMutablePair(eval("'(1 . 2)")).first = quote("a")
            fail()
        } catch (e: ImmutableException) {
        }

        try {
            (eval("'\"abc\"") as ScmString).set(0, 'b')
            fail()
        } catch (e: ImmutableException) {
        }

        try {
            (eval("'#(1 2 3)") as ScmVector).set(0, quote("a"))
            fail()
        } catch (e: ImmutableException) {
        }
    }


    /** 4.1.3 Procedure calls */
    fun test4_1_3() {
        check("(+ 3 4)", "7")
        check("((if #f + *) 3 4)", "12")

        try {
            eval("()")
            fail()
        } catch (e: CompileError) {
        }
    }


    /** 4.1.4 Procedures */
    fun test4_1_4() {
        assertTrue(eval("(lambda (x) (+ x x))") is Function)
        check("((lambda (x) (+ x x)) 4)", "8")

        eval(
            "(define reverse-subtract\n" +
                    "  (lambda (x y) (- y x)))"
        )
        check("(reverse-subtract 7 10)", "3")

        eval(
            "(define add4\n" +
                    "  (let ((x 4))\n" +
                    "    (lambda (y) (+ x y))))"
        )
        check("(add4 6)", "10")

        try {
            eval("(lambda (x y x) y)")
            fail()
        } catch (e: CompileError) {
        }

        check("((lambda x x) 3 4 5 6)", "(3 4 5 6)")
        check("((lambda (x y .z) z) 3 4 5 6)", "(5 6)")
    }


    /** 4.1.5 Conditionals */
    fun test4_1_5() {
        check("(if (> 3 2) 'yes 'no)", "yes")
        check("(if (> 2 3) 'yes 'no)", "no")
        check(
            "(if (> 3 2)\n" +
                    "    (- 3 2)\n" +
                    "    (+ 3 2))",
            "1"
        )
    }


    /** 4.1.6 Assignments */
    fun test4_1_6() {
        eval("(define x 2)")
        check("(+ x 1)", "3")
        eval("(set! x 4)")
        check("(+ x 1)", "5")
    }

    // 4.2 Derived expression types
    /** 4.2.1 Conditionals */
    fun no_test4_2_1_cond() {
        check(
            "(cond ((> 3 2) 'greater)\n" +
                    "      ((< 3 2) 'less))",
            "greater"
        )
        check(
            "(cond ((> 3 3) 'greater)\n" +
                    "      ((< 3 3) 'less))\n" +
                    "      (else 'equal))",
            "equal"
        )
        check(
            "(cond ((assv 'b '((a 1) (b 2))) => cadr)\n" +
                    "      (else #f))",
            "2"
        )
    }

    fun test4_2_1_and() {
        check(
            "(and (= 2 2) (> 2 1))",
            "#t"
        )
        check(
            "(and (= 2 2) (< 2 1))",
            "#f"
        )
        check(
            "(and 1 2 'c 'f '(f g))",
            "(f g)"
        )
        check(
            "(and)",
            "#t"
        )
    }

    fun test4_2_1_or() {
        check(
            "(or (= 2 2) (> 2 1))",
            "#t"
        )
        check(
            "(or (= 2 2) (< 2 1))",
            "#t"
        )
        check(
            "(or (memq 'b '(a b c)) (/ 3 0))",
            "(b c)"
        )
        check(
            "(or)",
            "#f"
        )
    }


    /** 4.2.2 Binding constructs */
    fun test4_2_2_let() {
        check(
            "(let ((x 2) (y 3))\n" +
                    "  (* x y))",
            "6"
        )

        check(
            "(let ((x 2) (y 3))\n" +
                    "  (let ((x 7)\n" +
                    "        (z (+ x y)))\n" +
                    "    (* z x)))",
            "35"
        )

        try {
            eval("(let ((x 1) (y x)) 0)")
            fail()
        } catch (e: SchemeException) {
        }

        try {
            eval("(let ((x y) (y 1)) 0)")
            fail()
        } catch (e: SchemeException) {
        }
    }

    fun test4_2_2_letstar() {
        check(
            "(let ((x 2) (y 3))\n" +
                    "  (let* ((x 7)\n" +
                    "         (z (+ x y)))\n" +
                    "    (* z x)))",
            "70"
        )

        check(
            "(let* ((a 1) (b (+ a 1)) (c (+ a b)) (d (+ b c)) (e (+ c d)))" +
                    "  (list a b c d e))",
            "(1 2 3 5 8)"
        )

        check("(let* ((x 1) (y x)) y)", "1")

        try {
            eval("(let* ((x y) (y 1)) 0)")
            fail()
        } catch (e: SchemeException) {
        }
    }

    fun test4_2_2_letrec() {
        check(
            "(letrec ((even?\n" +
                    "          (lambda (n)\n" +
                    "            (if (zero? n)\n" +
                    "                #t\n" +
                    "                (odd? (- n 1)))))\n" +
                    "         (odd?\n" +
                    "          (lambda (n)\n" +
                    "            (if (zero? n)\n" +
                    "                #f\n" +
                    "                (even? (- n 1))))))\n" +
                    "  (even? 88))\n",
            "#t"
        )

        check("(letrec ((x (lambda () y)) (y (lambda () x))) 0)", "0")
    }

    fun test4_2_2_common() {
        check("(let ((x 1)) (let    () (define x 2)) x)", "1")
        check("(let ((x 1)) (let*   () (define x 2)) x)", "1")
        check("(let ((x 1)) (letrec () (define x 2)) x)", "1")
    }


    /** 4.2.3 Sequencing */
    fun test4_2_3() {
        eval("(define x 0)")
        check(
            "(begin (set! x 5)\n" +
                    "       (+ x 1))",
            "6"
        )
    }


    /** 4.2.4 Iteration */
    fun test4_2_4_named_let() {
        check(
            "(let f ((x 3) (y 7))\n" +
                    "  (if (= x 0) y (f (- x 1) y)))",
            "7"
        )
    }

    /** 4.2.5 Delayed evaluation */
    /** 4.2.6 Quasiquotation */
    fun notest4_2_6() {
        check("`(list ,(+ 1 2) 4)", "(list 3 4)")
        check(
            "(let ((name 'a)) `(list ,name ',name))",
            "(list a (quote a))"
        )
        check(
            "`(a ,(+ 1 2) ,@(list 4 5 6) b)",
            "(a 3 4 5 6 b)"
        )
        check(
            "`#(10 5 ,(+ 1 1) ,@(list 4 3) 8)",
            "#(10 5 2 4 3 8)"
        )
        check(
            "`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)",
            "(a `(b ,(+ 1 2) ,(foo 4 d) e) f)"
        )
        check(
            "(let ((name1 'x) (name2 'y)) `(a `(b ,,name1 ,',name2 d) e))",
            "(a `(b ,x ,'y d) e))"
        )
    }


    /** 5.2 Definitions */
    fun test5_2() {
        try {
            eval("(define x 1 2 3)")
            fail()
        } catch (e: SyntaxArityError) {
        }

        try {
            eval("(set! x 1 2 3)")
            fail()
        } catch (e: SyntaxArityError) {
        }

        check(
            "(begin (define (f x) x x x x x) (f 1))",
            "1"
        )

        try {
            check(
                "(let ((a 1))" +
                        "  (define (f x)" +
                        "    (define b (+ a x))" +
                        "    (define a 5)" +
                        "    (+ a b))" +
                        "  (f 10))",
                "20"
            )
        } catch (e: SchemeException) {
        }
    }


    /** 5.3 Syntax definitions */
    fun test5_3() {
        eval(
            "(define-syntax static-cons " +
                    "  (lambda (def-env use-env . args)" +
                    "    (cons " +
                    "      use-env " +
                    "      (list 'quote (apply cons args)))))"
        )
        check(
            "(static-cons a b)",
            "(a . b)"
        )
    }


    /** 6.1 Equivalence predicates */
    fun test6_1() {
        // eqv?
        check("(eqv? 'a 'a)", "#t")
        check("(eqv? 'a 'b)", "#f")
        check("(eqv? 2 2)", "#t")
        check("(eqv? '() '())", "#t")
        check("(eqv? (cons 1 2) (cons 1 2))", "#f")
        check("(eqv? (lambda () 1) (lambda () 2))", "#f")
        check("(eqv? #f 'nil)", "#f")
        check("(let ((p (lambda (x) x))) (eqv? p p))", "#t")
    }


    // 6.2 Numbers
    /** 6.2.5 Numerical operations */
    fun testNumbers() {
        check("(+ 3 4)", "7")
        check("(+ 3)", "3")
        check("(+)", "0")
        check("(* 4)", "4")
        check("(*)", "1")

        check("(+)", "0")
        check("(*)", "1")

        check("(- 1)", "-1")
        check("(/ 1)", "1")

        check("(+ 3 3)", "6")
        check("(- 3 3)", "0")
        check("(* 3 3)", "9")
        check("(/ 3 3)", "1")

        check("(+  1 2 3 4)", "10")
        check("(-  1 2 3 4)", "-8")
        check("(*  1 2 3 4)", "24")
        check("(/ 24 2 3 4)", "1")
    }

    // 6.3 Other data types
    // 6.3.1 Booleans
    /** 6.3.2 Pairs and lists */
    fun test6_3_2() {
        // pair?
        check("(pair? '(a . b))", "#t")
        check("(pair? '(a b c))", "#t")
        check("(pair? '())", "#f")
        check("(pair? '#(a b))", "#f")

        // cons
        check("(cons 'a '())", "(a)")
        check("(cons '(a) '(b c d))", "((a) b c d)")
        check("(cons \"a\" '(b c))", "(\"a\" b c)")
        check("(cons 'a '3)", "(a . 3)")
        check("(cons '(a b) 'c)", "((a b) . c)")

        // car
        check("(car '(a b c))", "a")
        check("(car '((a) b c d))", "(a)")
        check("(car '(1 . 2))", "1")
        try {
            eval("(car '())")
            fail()
        } catch (e: SchemeException) {
        }

        // cdr
        check("(cdr '((a) b c d))", "(b c d)")
        check("(cdr '(1 . 2))", "2")
        try {
            eval("(cdr '())")
            fail()
        } catch (e: SchemeException) {
        }

        // set-car!
        eval("(define (f) (list 'not-a-constant-list))")
        eval("(define (g) '(constant-list))")
        eval("(set-car! (f) 3)")
        try {
            eval("(set-car! (g) 3)")
            fail()
        } catch (e: SchemeException) {
        }

        // set-cdr!
        eval("(set-cdr! (f) 3)")
        try {
            eval("(set-cdr! (g) 3)")
            fail()
        } catch (e: SchemeException) {
        }

        // list?
        check("(list? '(a b c))", "#t")
        check("(list? '())", "#t")
        check("(list? '(a . b))", "#f")

        // list
        check("(list 'a (+ 3 4) 'c)", "(a 7 c)")
        check("(list)", "()")

        // length
        check("(length '(a b c))", "3")
        check("(length '(a (b) (c d e)))", "3")
        check("(length '())", "0")

        // append
        check("(append '(x) '(y))", "(x y)")
        check("(append '(a) '(b c d))", "(a b c d)")
        check("(append '(a (b)) '((c)))", "(a (b) (c))")

        check("(append '(a b) '(c . d))", "(a b c . d)")
        check("(append '() 'a)", "a")

        // reverse
        check("(reverse '(a b c))", "(c b a)")
        check("(reverse '(a (b c) d (e (f))))", "((e (f)) d (b c) a)")

        // memq/memv/member
        check("(memq 'a '(a b c))", "(a b c)")
        check("(memq 'b '(a b c))", "(b c)")
        check("(memq 'a '(b c d))", "#f")
        check("(memq   (list 'a) '(b (a) c))", "#f")
        check("(member (list 'a) '(b (a) c))", "((a) c)")
        check("(memv '101 '(100 101 102))", "(101 102)")

        // assq/assv/assoc
        eval("(define e '((a 1) (b 2) (c 3)))")
        check("(assq 'a e)", "(a 1)")
        check("(assq 'b e)", "(b 2)")
        check("(assq 'd e)", "#f")
        check("(assq  (list 'a) '(((a)) ((b)) ((c))))", "#f")
        check("(assoc (list 'a) '(((a)) ((b)) ((c))))", "((a))")
        check("(assv 5 '((2 3) (5 7) (11 13)))", "(5 7)")
        check("(assq 'b '((a . 1) (b . 2)))", "(b . 2)")
    }


    /** 6.3.3 Symbols */
    fun test6_3_3() {
        // symbol?
        check("(symbol? 'foo)", "#t")
        check("(symbol? (car '(a b)))", "#t")
        check("(symbol? \"bar\")", "#f")
        check("(symbol? 'nil)", "#t")
        check("(symbol? '())", "#f")
        check("(symbol? #f)", "#f")

        // symbol->string
        // string->symbol
        check("(symbol->string 'flying-fish)", "\"flying-fish\"")
        check("(symbol->string 'Martin)", "\"martin\"")
        check("(symbol->string (string->symbol \"Malvia\"))", "\"Malvia\"")
    }


    /** 6.3.5 Strings */
    fun test6_3_5() {
        eval("(define (f) (make-string 3 #\\*))")
        eval("(define (g) \"***\"))")

        eval("(string-set! (f) 0 #\\?)")

        try {
            eval("(string-set! (g) 0 #\\?)")
            fail()
        } catch (e: SchemeException) {
        }

        try {
            eval("(string-set! (symbol->string 'immutable) 0 #\\?)")
            fail()
        } catch (e: SchemeException) {
        }
    }


    /** 6.3.6 Vectors */
    fun test6_3_6() {
        check("(vector-ref '#(1 1 2 3 5 8 13 21) 5)", "8")
        check("(vector 'a 'b 'c)", "#(a b c)")

        check("(vector-length '#(1 2 3 4))", "4")
    }


    /** 6.4 Control features */
    fun test6_4_procedureq() {
        check("(procedure?  car)", "#t")
        check("(procedure? 'car)", "#f")
        check("(procedure?  (lambda (x) (* x x)))", "#t")
        check("(procedure? '(lambda (x) (* x x)))", "#f")
        check("(call-with-current-continuation procedure?)", "#t")
    }

    fun test6_4_apply() {
        check("(apply + (list 3 4))", "7")
        check("(apply + 1 2 '(3 4))", "10")

        check(
            "(let ((l (list 1 2)))" +
                    "  (apply" +
                    "    (lambda x (set-car! x 3))" +
                    "    l)" +
                    "  l)",
            "(1 2)"
        )
    }

    fun test6_4_map() {
        check(
            "(map + '(1 2 3) '(4 5 6))",
            "(5 7 9)"
        )
    }

    fun test6_4_for_each() {
        eval("(define count 0)")
        check(
            "(for-each" +
                    "  (lambda (x y)" +
                    "    (set! count (+ count 1))" +
                    "    (+ x y count))" +
                    "  '(1 2 3)" +
                    "  '(4 5 6))",
            "(6 9 12)"
        )
    }

    fun test6_4_force() {
        check(
            "(force (delay (+ 1 2)))",
            "3"
        )

        check(
            "(let ((p (delay (+ 1 2)))) (list (force p) (force p)))",
            "(3 3)"
        )

        eval(
            "(begin" +
                    "  (define count 0)" +
                    "  (define x 'foo)" +
                    "  (define p" +
                    "    (delay" +
                    "      (begin" +
                    "        (set! count (+ count 1))" +
                    "        (if (> count x)" +
                    "          count" +
                    "          (force p)))))" +
                    "  (define x 5))"
        )

        check("(force p)", "6")
        eval("(set! x 10)")
        check("(force p)", "6")
    }

    fun test6_4_callcc() {
        check(
            "(let ((path '())\n" +
                    "      (c #f))\n" +
                    "  (let ((add (lambda (s)\n" +
                    "               (set! path (cons s path)))))\n" +
                    "    (dynamic-wind\n" +
                    "      (lambda () (add 'connect))\n" +
                    "      (lambda ()\n" +
                    "        (add (call-with-current-continuation\n" +
                    "               (lambda (c0)\n" +
                    "                 (set! c c0)\n" +
                    "                 'talk1))))\n" +
                    "      (lambda () (add 'disconnect)))\n" +
                    "    (if (< (length path) 4)\n" +
                    "        (c 'talk2)\n" +
                    "        (reverse path))))",
            "(connect talk1 disconnect\n" +
                    " connect talk2 disconnect)"
        )
    }


    /** 6.5 Eval */
    fun test6_5() {
        check("(eval '(* 7 3) (scheme-report-environment 5))", "21")
        check(
            "(let ((f (eval '(lambda (f x) (f x x))\n" +
                    "       (null-environment 5))))\n" +
                    "  (f + 10))",
            "20"
        )
    }


    /** additional stuff */
    fun testSpawn() {
        check("(spawn (lambda (c) 7))", "7")
        check(
            "(cons " +
                    "1 " +
                    "(spawn " +
                    "(lambda (c) " +
                    "(cons " +
                    "2 " +
                    "(c " +
                    "(lambda (k) " +
                    "(cons " +
                    "3 " +
                    "(k '()))))))))",
            "(1 3 2)"
        )

        try {
            eval("((spawn (lambda (c) c)) (lambda (k) k))")
            fail()
        } catch (e: SchemeException) {
        }

        try {
            eval(
                "(spawn (lambda (c) " +
                        "(c (lambda (k) " +
                        "(c (lambda (k) k))))))"
            )
            fail()
        } catch (e: SchemeException) {
        }

        eval(
            "(define id " +
                    "(spawn (lambda (c) " +
                    "(c (c (lambda (k) " +
                    "(k (lambda (k) k))))))))"
        )
        check("(id  45)", "45")
        check("(id '())", "()")
    }
}
