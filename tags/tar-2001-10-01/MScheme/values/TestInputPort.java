package MScheme.values;

import java.io.StringReader;

import MScheme.Value;


public class TestInputPort
            extends junit.framework.TestCase
{
    public final static String id
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

        assert(in.isReady());
        assert(in.peekChar() == 't');
        assert(in.readChar() == 't');
        assert(in.isReady());
        assert(in.peekChar() == 'e');
        assert(in.readChar() == 'e');
        assert(in.isReady());
        assert(in.readChar() == 's');
        assert(in.peekChar() == 't');
        assert(in.isReady());
        assert(in.readChar() == 't');
        assert(in.isReady());
        assert(in.peekChar() == InputPort.EOF);
        assert(in.readChar() == InputPort.EOF);
        assert(in.isReady());
    }

    public void testWS()
    throws Exception
    {
        StringReader source = new StringReader(
                                  " \t \n ; test# 1 2 \"\"\" ;;; 3 \n  \n ;  fkdjhgd "
                              );
        InputPort in = InputPort.create(source);

        assert(in.read().eq(in.EOF_VALUE));
    }

    public void testBool()
    throws Exception
    {
        StringReader source = new StringReader(" #t #f #t#f ");
        InputPort in = InputPort.create(source);

        assert(in.read().eq(ScmBoolean.createTrue()));
        assert(in.read().eq(ScmBoolean.createFalse()));
        assert(in.read().eq(ScmBoolean.createTrue()));
        assert(in.read().eq(ScmBoolean.createFalse()));
        assert(in.readChar() == ' ');
        assert(in.readChar() == InputPort.EOF);
    }

    public void testNumber()
    throws Exception
    {
        StringReader source = new StringReader(
                                  "-2 -1 0 1 2"
                              );
        InputPort in = InputPort.create(source);

        assert(in.read().eqv(ScmNumber.create(-2)));
        assert(in.read().eqv(ScmNumber.create(-1)));
        assert(in.read().eqv(ScmNumber.create( 0)));
        assert(in.read().eqv(ScmNumber.create( 1)));
        assert(in.read().eqv(ScmNumber.create( 2)));
        assert(in.readChar() == InputPort.EOF);
    }

    public void testChar()
    throws Exception
    {
        StringReader source = new StringReader(
                                  "#\\# #\\\\ #\\\" #\\  #\\newline #\\space #\\a"
                              );
        InputPort in = InputPort.create(source);

        assert(in.read().eqv(ScmChar.create('#')));
        assert(in.read().eqv(ScmChar.create('\\')));
        assert(in.read().eqv(ScmChar.create('"')));
        assert(in.read().eqv(ScmChar.create(' ')));
        assert(in.read().eqv(ScmChar.create('\n')));
        assert(in.read().eqv(ScmChar.create(' ')));
        assert(in.read().eqv(ScmChar.create('a')));
        assert(in.readChar() == InputPort.EOF);
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

        assert(in.read().equal(ScmString.create(str1)));
        assert(in.read().equal(ScmString.create(str2)));
        assert(in.read().equal(ScmString.create(str3b)));
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

        assert(in.read().equal(Empty.create()));
        assert(in.read().equal(Pair.create(one, two)));
        assert(in.read().equal(ListFactory.create(one, two, three)));
        assert(in.read().equal(ListFactory.create(one, two, three)));
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

        assert(in.read().equal(ScmVector.create()));
        assert(in.read().equal(ScmVector.create(2, one)));
        assert(in.read().equal(v));
    }

    public void testSymbol()
    throws Exception
    {
        Value test = Symbol.create("hallo");
        StringReader source = new StringReader(
                                  "Hallo hallo HALLO HaLlO hAlLo + - ... ?12"
                              );
        InputPort in = InputPort.create(source);

        assert(in.read().eq(test));
        assert(in.read().eq(test));
        assert(in.read().eq(test));
        assert(in.read().eq(test));
        assert(in.read().eq(test));
        assert(in.read().eq(Symbol.create("+")));
        assert(in.read().eq(Symbol.create("-")));
        assert(in.read().eq(Symbol.create("...")));
        assert(in.read().eq(Symbol.create("?12")));
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

        assert(in.read().equal(ListFactory.create(q,   a)));
        assert(in.read().equal(ListFactory.create(q,   a)));
        assert(in.read().equal(ListFactory.create(qq,  a)));
        assert(in.read().equal(ListFactory.create(uq,  a)));
        assert(in.read().equal(ListFactory.create(uqs, a)));
    }
}
