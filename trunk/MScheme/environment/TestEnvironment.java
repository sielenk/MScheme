/* Some junit tests for MSchemes environments.
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

package MScheme.environment;

import MScheme.Value;
import MScheme.Code;
import MScheme.Syntax;

import MScheme.environment.*;
import MScheme.values.*;
import MScheme.code.*;
import MScheme.syntax.SyntaxFactory;
import MScheme.exceptions.*;


public class TestEnvironment
    extends junit.framework.TestCase
{
    public final static String id
        = "$Id$";


    protected Environment env;
    protected Symbol sym1;
    protected Symbol sym2;
    protected Value  val1;
    protected Value  val2;

    public TestEnvironment(String name)
    {
        super(name);
    }

    protected void setUp()
    {
        env = Environment.getEmpty();

        sym1 = Symbol.create("test1");
        sym2 = Symbol.create("test2");

        val1 = Empty.create();
        val2 = ScmBoolean.createTrue();
    }

    protected void tearDown()
    {
        env = null;
        sym1 = sym2 = null;
        val1 = val2 = null;
    }


    public void testTestPattern()
    {
        assert("different symbols are equal (==)", sym1 != sym2);
        assert("different symbols are equal (equals)", !sym1.equals(sym2));
        assert("different entities are equal (==)", val1 != val2);
        assert("different entities are equals (equals)", !val1.equals(val2));
    }

    public void testNormal()
    throws Exception
    {
        try
        {
            env.lookup(sym1);
            fail("env not empty");
        }
        catch (SymbolNotFoundException e)
        { }

        try
        {
            env.assign(sym1, val1);
            fail("expected SymbolNotFound exception");
        }
        catch (SymbolNotFoundException e)
        { }

        env.define(sym1, val1);

        assert(
            "lookup failed",
            env.lookup(sym1) == val1
        );

        env.assign(sym1, val2);

        assert(
            "assign failed",
            env.lookup(sym1) == val2
        );
    }

    public void testChild()
    throws Exception
    {
        Environment child  = env.newChild();

        assert(child != env);

        env  .define(sym1, val1);
        child.define(sym2, val2);

        assert(child.lookup(sym1) == val1);
        assert(child.lookup(sym2) == val2);
        assert(env  .lookup(sym1) == val1);

        try
        {
            env.lookup(sym2);
            fail("expected SymbolNotFoundException");
        }
        catch (SymbolNotFoundException e)
        { }

        env.define(sym2, val1);
        assert(child.lookup(sym2) == val2);
        assert(env  .lookup(sym2) == val1);
    }

    public void testSyntax()
    throws Exception
    {
        StaticEnvironment env = new StaticEnvironment();

        try
        {
            env.getSyntaxFor(sym1);
            fail("expected SymbolNotFoundException");
        }
        catch (SymbolNotFoundException e)
        { }

        try
        {
            env.getSyntaxFor(sym1);
            fail("expected SymbolNotFoundException");
        }
        catch (SymbolNotFoundException e)
        { }

        Syntax    token = SyntaxFactory.getBeginToken();
        env.defineSyntax(sym1, token);

        assert(env.getSyntaxFor(sym1) == token);

        try
        {
            env.getReferenceFor(sym1);
            fail("expected UnexpectedSyntax");
        }
        catch (UnexpectedSyntax e)
        { }

        Reference reference = env.define(sym2);

        assert(env.getReferenceFor (sym2) == reference);
    }

    public void testExtendedStatic()
    throws Exception
    {
        env.getStatic().define(sym1);

        try
        {
            env.lookup(sym1);
            fail("expected UninitializedSymbolException");
        }
        catch (RuntimeError e)
        { }

        env.assign(sym1, val1);

        assert(
            env.lookup(sym1) == val1
        );
    }
}

