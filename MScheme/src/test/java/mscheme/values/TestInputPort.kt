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


public class TestInputPort
    extends junit.framework.TestCase {

  public TestInputPort(String name) {
    super(name);
  }


  public void testCharIO()
      throws Exception {
    StringReader source = new StringReader("test");
    InputPort in = InputPort.Companion.create(source);

    assertTrue(in.isReady());
    assertEquals('t', in.peekChar());
    assertEquals('t', in.readChar());
    assertTrue(in.isReady());
    assertEquals('e', in.peekChar());
    assertEquals('e', in.readChar());
    assertTrue(in.isReady());
    assertEquals('s', in.readChar());
    assertEquals('t', in.peekChar());
    assertTrue(in.isReady());
    assertEquals('t', in.readChar());
    assertTrue(in.isReady());
    assertEquals(in.peekChar(), InputPort.EOF);
    assertEquals(in.readChar(), InputPort.EOF);
    assertTrue(in.isReady());
  }

  public void testWS()
      throws Exception {
    StringReader source = new StringReader(
        " \t \n ; test# 1 2 \"\"\" ;;; 3 \n  \n ;  fkdjhgd "
    );
    InputPort in = InputPort.Companion.create(source);

    assertTrue(ValueTraits.INSTANCE.eq(in.read(), InputPort.Companion.getEOF_VALUE()));
  }

  public void testBool()
      throws Exception {
    StringReader source = new StringReader(" #t #f #t#f ");
    InputPort in = InputPort.Companion.create(source);

    assertTrue(ValueTraits.INSTANCE.eq(in.read(), ValueTraits.TRUE));
    assertTrue(ValueTraits.INSTANCE.eq(in.read(), ValueTraits.FALSE));
    assertTrue(ValueTraits.INSTANCE.eq(in.read(), ValueTraits.TRUE));
    assertTrue(ValueTraits.INSTANCE.eq(in.read(), ValueTraits.FALSE));
    assertEquals(' ', in.readChar());
    assertEquals(in.readChar(), InputPort.EOF);
  }

  public void testNumber()
      throws Exception {
    StringReader source = new StringReader(
        "-2 -1 0 1 2"
    );
    InputPort in = InputPort.Companion.create(source);

    assertTrue(ValueTraits.INSTANCE.eqv(in.read(), ScmNumber.Companion.create(-2)));
    assertTrue(ValueTraits.INSTANCE.eqv(in.read(), ScmNumber.Companion.create(-1)));
    assertTrue(ValueTraits.INSTANCE.eqv(in.read(), ScmNumber.Companion.create(0)));
    assertTrue(ValueTraits.INSTANCE.eqv(in.read(), ScmNumber.Companion.create(1)));
    assertTrue(ValueTraits.INSTANCE.eqv(in.read(), ScmNumber.Companion.create(2)));
    assertEquals(in.readChar(), InputPort.EOF);
  }

  public void testChar()
      throws Exception {
    StringReader source = new StringReader(
        "#\\# #\\\\ #\\\" #\\  #\\newline #\\space #\\a"
    );
    InputPort in = InputPort.Companion.create(source);

    assertTrue(ValueTraits.INSTANCE.eqv(in.read(), ValueTraits.INSTANCE.toScmChar('#')));
    assertTrue(ValueTraits.INSTANCE.eqv(in.read(), ValueTraits.INSTANCE.toScmChar('\\')));
    assertTrue(ValueTraits.INSTANCE.eqv(in.read(), ValueTraits.INSTANCE.toScmChar('"')));
    assertTrue(ValueTraits.INSTANCE.eqv(in.read(), ValueTraits.INSTANCE.toScmChar(' ')));
    assertTrue(ValueTraits.INSTANCE.eqv(in.read(), ValueTraits.INSTANCE.toScmChar('\n')));
    assertTrue(ValueTraits.INSTANCE.eqv(in.read(), ValueTraits.INSTANCE.toScmChar(' ')));
    assertTrue(ValueTraits.INSTANCE.eqv(in.read(), ValueTraits.INSTANCE.toScmChar('a')));
    assertEquals(in.readChar(), InputPort.EOF);
  }

  public void testString()
      throws Exception {
    String str1 = "";
    String str2 = " Hallo ! ";
    String str3a = " \\\\ \\\" \n ";
    String str3b = " \\ \" \n ";

    StringReader source = new StringReader(
        '"' + str1 + '"' + ' '
            + '"' + str2 + '"' + ' '
            + '"' + str3a + '"'
    );
    InputPort in = InputPort.Companion.create(source);

    assertTrue(ValueTraits.INSTANCE.equal(in.read(), ScmString.Companion.create(str1)));
    assertTrue(ValueTraits.INSTANCE.equal(in.read(), ScmString.Companion.create(str2)));
    assertTrue(ValueTraits.INSTANCE.equal(in.read(), ScmString.Companion.create(str3b)));
  }

  public void testList()
      throws Exception {
    Object one = ScmNumber.Companion.create(1);
    Object two = ScmNumber.Companion.create(2);
    Object three = ScmNumber.Companion.create(3);

    StringReader source = new StringReader(
        "()(1 .2)(1 2 3)(1 .(2 .(3 .())))"
    );
    InputPort in = InputPort.Companion.create(source);

    assertTrue(ValueTraits.INSTANCE.equal(in.read(), ListFactory.INSTANCE.create()));
    assertTrue(ValueTraits.INSTANCE.equal(in.read(), ListFactory.INSTANCE.createPair(one, two)));
    assertTrue(
        ValueTraits.INSTANCE.equal(in.read(), ListFactory.INSTANCE.create(one, two, three)));
    assertTrue(
        ValueTraits.INSTANCE.equal(in.read(), ListFactory.INSTANCE.create(one, two, three)));
  }

  public void testVector()
      throws Exception {
    Object one = ScmNumber.Companion.create(1);
    Object two = ScmNumber.Companion.create(2);
    ScmVector v = ScmVector.Companion.create(3, one);
    v.set(2, two);
    StringReader source = new StringReader(
        "#() #(1 1) #(1 1 2)"
    );
    InputPort in = InputPort.Companion.create(source);

    assertTrue(ValueTraits.INSTANCE.equal(in.read(), ScmVector.Companion.create()));
    assertTrue(ValueTraits.INSTANCE.equal(in.read(), ScmVector.Companion.create(2, one)));
    assertTrue(ValueTraits.INSTANCE.equal(in.read(), v));
  }

  public void testSymbol()
      throws Exception {
    Object test = "hallo";
    StringReader source = new StringReader(
        "Hallo hallo HALLO HaLlO hAlLo + - ... ?12"
    );
    InputPort in = InputPort.Companion.create(source);

    assertTrue(ValueTraits.INSTANCE.eq(in.read(), test));
    assertTrue(ValueTraits.INSTANCE.eq(in.read(), test));
    assertTrue(ValueTraits.INSTANCE.eq(in.read(), test));
    assertTrue(ValueTraits.INSTANCE.eq(in.read(), test));
    assertTrue(ValueTraits.INSTANCE.eq(in.read(), test));
    assertTrue(ValueTraits.INSTANCE.eq(in.read(), "+"));
    assertTrue(ValueTraits.INSTANCE.eq(in.read(), "-"));
    assertTrue(ValueTraits.INSTANCE.eq(in.read(), "..."));
    assertTrue(ValueTraits.INSTANCE.eq(in.read(), "?12"));
  }

  public void testAbbrev()
      throws Exception {
    StringReader source = new StringReader(
        "'a ' a `a ,a ,@a"
    );
    InputPort in = InputPort.Companion.create(source);

    String a = "a";
    String q = "quote";
    String qq = "quasiquote";
    String uq = "unquote";
    String uqs = "unquote-splicing";

    assertTrue(ValueTraits.INSTANCE.equal(in.read(), ListFactory.INSTANCE.create(q, a)));
    assertTrue(ValueTraits.INSTANCE.equal(in.read(), ListFactory.INSTANCE.create(q, a)));
    assertTrue(ValueTraits.INSTANCE.equal(in.read(), ListFactory.INSTANCE.create(qq, a)));
    assertTrue(ValueTraits.INSTANCE.equal(in.read(), ListFactory.INSTANCE.create(uq, a)));
    assertTrue(ValueTraits.INSTANCE.equal(in.read(), ListFactory.INSTANCE.create(uqs, a)));
  }
}
