package MScheme.values;

import java.io.StringReader;


public class TestInputPort
    extends junit.framework.TestCase
{
    public TestInputPort(String name)
    { super(name); }


    public void testCharIO()
        throws Exception
    {
        StringReader source = new StringReader("test");
        InputPort in = ValueFactory.createInputPort(source);
        
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
        InputPort in = ValueFactory.createInputPort(source);
        
        assert(in.read().eq(in.EOF_VALUE));
    }
    
    public void testBool()
        throws Exception
    {
        StringReader source = new StringReader(" #t #f #t#f ");
        InputPort in = ValueFactory.createInputPort(source);
        
        assert(in.read().eq(ValueFactory.createTrue()));
        assert(in.read().eq(ValueFactory.createFalse()));
        assert(in.read().eq(ValueFactory.createTrue()));
        assert(in.read().eq(ValueFactory.createFalse()));
        assert(in.readChar() == ' ');
        assert(in.readChar() == InputPort.EOF);
    }

    public void testNumber()
        throws Exception
    {
        StringReader source = new StringReader(
            "-2 -1 0 1 2"
        );
        InputPort in = ValueFactory.createInputPort(source);
        
        assert(in.read().eqv(ValueFactory.createNumber(-2)));
        assert(in.read().eqv(ValueFactory.createNumber(-1)));
        assert(in.read().eqv(ValueFactory.createNumber( 0)));
        assert(in.read().eqv(ValueFactory.createNumber( 1)));
        assert(in.read().eqv(ValueFactory.createNumber( 2)));
        assert(in.readChar() == InputPort.EOF);
    }
    
    public void testChar()
        throws Exception
    {
        StringReader source = new StringReader(
            "#\\# #\\\\ #\\\" #\\  #\\newline #\\space #\\a"
        );
        InputPort in = ValueFactory.createInputPort(source);
        
        assert(in.read().eqv(ValueFactory.createChar('#')));
        assert(in.read().eqv(ValueFactory.createChar('\\')));
        assert(in.read().eqv(ValueFactory.createChar('"')));
        assert(in.read().eqv(ValueFactory.createChar(' ')));
        assert(in.read().eqv(ValueFactory.createChar('\n')));
        assert(in.read().eqv(ValueFactory.createChar(' ')));
        assert(in.read().eqv(ValueFactory.createChar('a')));
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
        InputPort in = ValueFactory.createInputPort(source);
        
        assert(in.read().equal(ValueFactory.createString(str1)));
        assert(in.read().equal(ValueFactory.createString(str2)));
        assert(in.read().equal(ValueFactory.createString(str3b)));
    }

    public void testList()
        throws Exception
    {
        Value one   = ValueFactory.createNumber(1);
        Value two   = ValueFactory.createNumber(2);
        Value three = ValueFactory.createNumber(3);
        
        StringReader source = new StringReader(
            "()(1 .2)(1 2 3)(1 .(2 .(3 .())))"
        );
        InputPort in = ValueFactory.createInputPort(source);
        
        assert(in.read().equal(Empty.create()));
        assert(in.read().equal(Pair.create(one, two)));
        assert(in.read().equal(ValueFactory.createList(one, two, three)));
        assert(in.read().equal(ValueFactory.createList(one, two, three)));
    }

    public void testVector()
        throws Exception
    {
        Value one   = ValueFactory.createNumber(1);
        Value two   = ValueFactory.createNumber(2);
        SchemeVector v = ValueFactory.createVector(3, one);
        v.set(2, two);
        StringReader source = new StringReader(
            "#() #(1 1) #(1 1 2)"
        );
        InputPort in = ValueFactory.createInputPort(source);
        
        assert(in.read().equal(ValueFactory.createVector()));
        assert(in.read().equal(ValueFactory.createVector(2, one)));
        assert(in.read().equal(v));
    }
    
    public void testSymbol()
        throws Exception
    {
        Value test = ValueFactory.createSymbol("hallo");
        StringReader source = new StringReader(
            "Hallo hallo HALLO HaLlO hAlLo + - ... ?12"
        );
        InputPort in = ValueFactory.createInputPort(source);
        
        assert(in.read().eq(test));
        assert(in.read().eq(test));
        assert(in.read().eq(test));
        assert(in.read().eq(test));
        assert(in.read().eq(test));
        assert(in.read().eq(ValueFactory.createSymbol("+")));
        assert(in.read().eq(ValueFactory.createSymbol("-")));
        assert(in.read().eq(ValueFactory.createSymbol("...")));
        assert(in.read().eq(ValueFactory.createSymbol("?12")));
    }

    public void testAbbrev()
        throws Exception
    {
        Value test = ValueFactory.createSymbol("hallo");
        StringReader source = new StringReader(
            "'a ' a `a ,a ,@a"
        );
        InputPort in = ValueFactory.createInputPort(source);

        Symbol a   = ValueFactory.createSymbol("a");
        Symbol q   = ValueFactory.createSymbol("quote");
        Symbol qq  = ValueFactory.createSymbol("quasiquote");
        Symbol uq  = ValueFactory.createSymbol("unquote");
        Symbol uqs = ValueFactory.createSymbol("unquote-splicing");
    
        assert(in.read().equal(ValueFactory.createList(q,   a)));
        assert(in.read().equal(ValueFactory.createList(q,   a)));
        assert(in.read().equal(ValueFactory.createList(qq,  a)));
        assert(in.read().equal(ValueFactory.createList(uq,  a)));
        assert(in.read().equal(ValueFactory.createList(uqs, a)));
    }
}

