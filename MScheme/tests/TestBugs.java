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

package MScheme.tests;

import junit.extensions.ExceptionTestCase;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import MScheme.exceptions.CompileError;
import MScheme.exceptions.SchemeException;


public class TestBugs
    extends TestSchemeBase
{
    public final static String id
        = "$Id$";

    public TestBugs(String name)
    {
        super(name);
    }


    public void test_2002_19_03()
        throws SchemeException
    {
        // This failed, because set! didn't use delayed
        // references, MHS 2002-19-03
        check("(begin\n" +
              "  (define (f)\n" +
              "    (define (g)\n" +
              "      (set! x 1)\n" +
              "      x)\n" +
              "    (define x 2)\n" +
              "    (g)\n" +
              "    x)\n" +
              "  (f))",
              "1"
        );
    }    

    public void test_2002_04_09()
        throws SchemeException
    {
        // It is now illegal to internally redefine a symbol.
        try {
            eval("(let ((x 1)) (begin (define x 2)) x)");
            fail();
        }
        catch (CompileError e)
        { }
    }

    public void test_2002_04_15a()
        throws SchemeException
    {
        // Internal definitions are not allowed after the
        // first expression.


        try {
            eval("(let () 1 (define x 2) x)");
            fail();
        }
        catch (CompileError e)
        { }

        try {
            eval("(let () 'a (define x 2) x)");
            fail();
        }
        catch (CompileError e)
        { }

        try {
            eval("(let ((a 1)) a (define x 2) x)");
            fail();
        }
        catch (CompileError e)
        { }

        try {
            eval("(let () (if 1 1 1) (define x 2) x)");
            fail();
        }
        catch (CompileError e)
        { }
    }

    public void test_2002_04_15ba()
        throws SchemeException
    {
        // no nested definitions
        try {
            eval("(define x (define y 3))");
            fail();
        }
        catch (CompileError e)
        { }
    }

    public void test_2002_04_15bb()
        throws SchemeException
    {
        // no nested definitions
        try {
            eval("(lambda () (cons (define a 1) 2))");
            fail();
        }
        catch (CompileError e)
        { }
    }

    public void test_2002_04_15c()
        throws SchemeException
    {
        try {
            eval("(define (f)\n" +
                 "  (define y 2)\n" +
                 "  (define g (+ y 1))\n" +
                 "  g\n" +
                 "  y))"
            );
            fail();
        }
        catch (CompileError e)
        { }

        try {
            eval("(define (f)\n" +
                 "  (define g (+ y 1))\n" +
                 "  (define y 2)\n" +
                 "  g\n" +
                 "  y))"
            );
            fail();
        }
        catch (CompileError e)
        { }

        try {
            eval("(define (f)\n" +
                 "  (define y 2)\n" +
                 "  (define g (set! y 1))\n" +
                 "  g\n" +
                 "  y))"
            );
            fail();
        }
        catch (CompileError e)
        { }
    }

    public void test_todo01()
        throws SchemeException
    {
        try {
            eval("(cons (define a 1) 2)");
            fail();
        }
        catch (CompileError e)
        { }
    }
}
