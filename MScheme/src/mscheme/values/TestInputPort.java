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

package mscheme.values;

import java.io.StringReader;

import mscheme.Value;

public class TestInputPort
            extends junit.framework.TestCase
{
    public final static String CVS_ID
    = "$Id$";


    public TestInputPort(String name)
    {
        super(name);
    }


    public void testCharIO()
    throws Exception
    {
        StringReader source = new StringReader("test");
        InputPort in = InputPort.create(source);

        assertTrue(in.isReady());
        assertTrue(in.peekChar() == 't');
        assertTrue(in.readChar() == 't');
        assertTrue(in.isReady());
        assertTrue(in.peekChar() == 'e');
        assertTrue(in.readChar() == 'e');
        assertTrue(in.isReady());
        assertTrue(in.readChar() == 's');
        assertTrue(in.peekChar() == 't');
        assertTrue(in.isReady());
        assertTrue(in.readChar() == 't');
        assertTrue(in.isReady());
        assertTrue(in.peekChar() == InputPort.EOF);
        assertTrue(in.readChar() == InputPort.EOF);
        assertTrue(in.isReady());
    }

    public void testWS()
    throws Exception
    {
        StringReader source = new StringReader(
                                  " \t \n ; test# 1 2 \"\"\" ;;; 3 \n  \n ;  fkdjhgd "
                              );
        InputPort in = InputPort.create(source);

        assertTrue(ValueTraits.eq(in.read(), InputPort.EOF_VALUE));
    }

    public void testBool()
    throws Exception
    {
        StringReader source = new StringReader(" #t #f #t#f ");
        InputPort in = InputPort.create(source);

        assertTrue(ValueTraits.eq(in.read(), ValueTraits.TRUE));
        assertTrue(ValueTraits.eq(in.read(), ValueTraits.FALSE));
        assertTrue(ValueTraits.eq(in.read(), ValueTraits.TRUE));
        assertTrue(ValueTraits.eq(in.read(), ValueTraits.FALSE));
        assertTrue(in.readChar() == ' ');
        assertTrue(in.readChar() == InputPort.EOF);
    }

    public void testNumber()
    throws Exception
    {
        StringReader source = new StringReader(
                                  "-2 -1 0 1 2"
                              );
        InputPort in = InputPort.create(source);

        assertTrue(ValueTraits.eqv(in.read(), ScmNumber.create(-2)));
        assertTrue(ValueTraits.eqv(in.read(), ScmNumber.create(-1)));
        assertTrue(ValueTraits.eqv(in.read(), ScmNumber.create( 0)));
        assertTrue(ValueTraits.eqv(in.read(), ScmNumber.create( 1)));
        assertTrue(ValueTraits.eqv(in.read(), ScmNumber.create( 2)));
        assertTrue(in.readChar() == InputPort.EOF);
    }

    public void testChar()
    throws Exception
    {
        StringReader source = new StringReader(
                                  "#\\# #\\\\ #\\\" #\\  #\\newline #\\space #\\a"
                              );
        InputPort in = InputPort.create(source);

        assertTrue(ValueTraits.eqv(in.read(), ValueTraits.toScmChar('#')));
        assertTrue(ValueTraits.eqv(in.read(), ValueTraits.toScmChar('\\')));
        assertTrue(ValueTraits.eqv(in.read(), ValueTraits.toScmChar('"')));
        assertTrue(ValueTraits.eqv(in.read(), ValueTraits.toScmChar(' ')));
        assertTrue(ValueTraits.eqv(in.read(), ValueTraits.toScmChar('\n')));
        assertTrue(ValueTraits.eqv(in.read(), ValueTraits.toScmChar(' ')));
        assertTrue(ValueTraits.eqv(in.read(), ValueTraits.toScmChar('a')));
        assertTrue(in.readChar() == InputPort.EOF);
    }

    public void testString()
    throws Exception
    {
        String str1  = "";
        String str2  = " Hallo ! ";
        String str3a = " \\\\ \\\" \n ";
        String str3b = " \\ \" \n ";

        StringReader source = new StringReader(
                                  '"' + str1  + '"' + ' '
                                  + '"' + str2  + '"' + ' '
                                  + '"' + str3a + '"'
                              );
        InputPort in = InputPort.create(source);

        assertTrue(ValueTraits.equal(in.read(), ScmString.create(str1)));
        assertTrue(ValueTraits.equal(in.read(), ScmString.create(str2)));
        assertTrue(ValueTraits.equal(in.read(), ScmString.create(str3b)));
    }

    public void testList()
    throws Exception
    {
        Value one   = ScmNumber.create(1);
        Value two   = ScmNumber.create(2);
        Value three = ScmNumber.create(3);

        StringReader source = new StringReader(
                                  "()(1 .2)(1 2 3)(1 .(2 .(3 .())))"
                              );
        InputPort in = InputPort.create(source);

        assertTrue(ValueTraits.equal(in.read(), ListFactory.create()));
        assertTrue(ValueTraits.equal(in.read(), ListFactory.createPair(one, two)));
        assertTrue(ValueTraits.equal(in.read(), ListFactory.create(one, two, three)));
        assertTrue(ValueTraits.equal(in.read(), ListFactory.create(one, two, three)));
    }

    public void testVector()
    throws Exception
    {
        Value one   = ScmNumber.create(1);
        Value two   = ScmNumber.create(2);
        ScmVector v = ScmVector.create(3, one);
        v.set(2, two);
        StringReader source = new StringReader(
                                  "#() #(1 1) #(1 1 2)"
                              );
        InputPort in = InputPort.create(source);

        assertTrue(ValueTraits.equal(in.read(), ScmVector.create()));
        assertTrue(ValueTraits.equal(in.read(), ScmVector.create(2, one)));
        assertTrue(ValueTraits.equal(in.read(), v));
    }

    public void testSymbol()
    throws Exception
    {
        Value test = Symbol.create("hallo");
        StringReader source = new StringReader(
                                  "Hallo hallo HALLO HaLlO hAlLo + - ... ?12"
                              );
        InputPort in = InputPort.create(source);

        assertTrue(ValueTraits.eq(in.read(), test));
        assertTrue(ValueTraits.eq(in.read(), test));
        assertTrue(ValueTraits.eq(in.read(), test));
        assertTrue(ValueTraits.eq(in.read(), test));
        assertTrue(ValueTraits.eq(in.read(), test));
        assertTrue(ValueTraits.eq(in.read(), Symbol.create("+")));
        assertTrue(ValueTraits.eq(in.read(), Symbol.create("-")));
        assertTrue(ValueTraits.eq(in.read(), Symbol.create("...")));
        assertTrue(ValueTraits.eq(in.read(), Symbol.create("?12")));
    }

    public void testAbbrev()
    throws Exception
    {
        Value test = Symbol.create("hallo");
        StringReader source = new StringReader(
                                  "'a ' a `a ,a ,@a"
                              );
        InputPort in = InputPort.create(source);

        Symbol a   = Symbol.create("a");
        Symbol q   = Symbol.create("quote");
        Symbol qq  = Symbol.create("quasiquote");
        Symbol uq  = Symbol.create("unquote");
        Symbol uqs = Symbol.create("unquote-splicing");

        assertTrue(ValueTraits.equal(in.read(), ListFactory.create(q,   a)));
        assertTrue(ValueTraits.equal(in.read(), ListFactory.create(q,   a)));
        assertTrue(ValueTraits.equal(in.read(), ListFactory.create(qq,  a)));
        assertTrue(ValueTraits.equal(in.read(), ListFactory.create(uq,  a)));
        assertTrue(ValueTraits.equal(in.read(), ListFactory.create(uqs, a)));
    }
}
