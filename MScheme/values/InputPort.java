package MScheme.values;

import java.io.Writer;
import java.io.Reader;
import java.io.FileReader;
import java.io.InputStreamReader;
import java.io.PushbackReader;
import java.io.IOException;

import MScheme.exceptions.OpenException;
import MScheme.exceptions.ReadException;
import MScheme.exceptions.ParseException;
import MScheme.exceptions.CloseException;
import MScheme.exceptions.ImmutableException;
import MScheme.exceptions.InvalidVectorIndexException;

class EofValue
    extends SelfEvaluatingValue
{
    public final static EofValue INSTANCE = new EofValue();
    
    private EofValue() { }
    
    public void write(Writer dest)
        throws IOException
    { dest.write("[eof]"); }
}


public class InputPort
    extends Port
{
    public final static int   EOF       = -1;
    public final static Value EOF_VALUE = EofValue.INSTANCE;

    
    private final PushbackReader _reader;
        
    private InputPort(PushbackReader reader)
    { _reader = reader; }


    public static InputPort create(Reader reader)
    {
        return new InputPort(
            reader instanceof PushbackReader
            ? (PushbackReader)reader
            : new PushbackReader(reader)
        );
    }

    public static InputPort create(SchemeString filename)
        throws OpenException
    { return create(filename.getJavaString()); }

    public static InputPort create(String filename)
        throws OpenException
    {
        try {
            return create(new FileReader(filename));
        }
        catch (IOException e) {
            throw new OpenException(
                ValueFactory.createString(filename)
            );
        }
    }

    public static InputPort create()
    { return create(new InputStreamReader(System.in)); }
    

    // specialisation of Port
    
    public boolean isInput()
    { return true; }
    
    public InputPort toInput()
    { return this; }


    public void close()
        throws CloseException
    {
        try {
            _reader.close();
        }
        catch (IOException e) {
            throw new CloseException(this);
        }
    }


    // input port    
    
    public Value read()
        throws ReadException, ParseException
    {
        try {
            return parseDatum().setLiteral();
        }
        catch (IOException e) {
            throw new ReadException(this);
        }
    }

    
    private boolean isWhitespace(int c) 
    { return (c == ' ') || (c == '\t') || (c == '\n'); }

    private boolean isDelimiter(int c)  
    {
        return isWhitespace(c)
            || (c == '(')
            || (c == ')')
            || (c == '"')
            || (c == ';');
    }

    private int skipWSread()
        throws IOException
    {
        boolean inComment = false;
    
        for (;;) {
            int c = _reader.read();

            if (inComment) {
                switch (c) {
                case '\n':
                    inComment = false;
                    break;

                case EOF:
                    return c;
                }
            } else {
                if (c == ';') {
                    inComment = true;
                } else if (!isWhitespace(c)) {
                    return c;
                }
            }
        }
    }

    private char readNoEof()
        throws IOException, ParseException
    {
        int c = _reader.read();
        if (c == EOF) {
            throw new ParseException(
                this,
                "unexpected EOF"
            );
        }
        return (char)c;
    }

    private SchemeChar parseChar()
        throws IOException, ParseException
    {
        int c = readNoEof();
        
        if ((c == 's') || (c == 'n')) {
            StringBuffer charName = new StringBuffer();
            
            do {
                charName.append((char)c);
                c = _reader.read();
            } while ((c != EOF) && !isDelimiter(c));
            
            if (c != EOF) {
                _reader.unread(c);
            }
            
            String name = charName.toString();
            
            if (name.equals("s")) {
                return ValueFactory.createChar('s');
            } else if (name.equals("n")) {
                return ValueFactory.createChar('n');
            } else if (name.equals("space")) {
                return ValueFactory.createChar(' ');
            } else if (name.equals("newline")) {
                return ValueFactory.createChar('\n');
            } else {
                throw new ParseException(
                    this,
                    "invalid character name '" + name + "'"
                );
            }
        } else {
            return ValueFactory.createChar((char)c);
        }
    }
    
    private SchemeVector parseVector(int index)
        throws IOException, ParseException
    {
        int c = skipWSread();
    
        if (c == ')') {
            return ValueFactory.createVector(index);
        } if (c == EOF) {
            throw new ParseException(
                this,
                "unexpected EOF"
            );
        } else {
            _reader.unread(c);
            
            Value        head   = parseDatum();
            SchemeVector result = parseVector(index + 1);
            
            try {
                result.set(index, head);
            }
            catch (InvalidVectorIndexException e) {
                throw new RuntimeException(
                    "unexpected InvalidVectorIndexException in "
                    + "InputPort.parseVector"
                );
            }
            catch (ImmutableException e) {
                throw new RuntimeException(
                    "unexpected ImmutableException in "
                    + "InputPort.parseVector"
                );
            }
            
            return result;
        }
    }
    
    private List parseList()
        throws IOException, ParseException
    {
        int c = skipWSread();
    
        if (c == ')') {
            return ValueFactory.createList();
        } if (c == EOF) {
            throw new ParseException(
                this,
                "unexpected EOF"
            );
        } else {
            _reader.unread(c);
            Value head = parseDatum();
            
            int la1 = skipWSread();
            if (la1 == '.') {
                List result = ValueFactory.createPair(
                    head,
                    parseDatum()
                );
            
                if (skipWSread() != ')') {
                    throw new ParseException(
                        this,
                        "expected ')'"
                    );
                }
                
                return result;
            } else {
                _reader.unread(la1);
                return ValueFactory.createPair(
                    head,
                    parseList()
                );
            }
        }
    }
    
    private SchemeString parseString()
        throws IOException, ParseException
    {
        StringBuffer buf = new StringBuffer();
    
        for (;;) {
            char c = readNoEof();
        
            switch (c) {
            case '"':
                return ValueFactory.createString(
                    buf.toString()
                );
                
            case '\\':
                buf.append(readNoEof());
                break;

            default:
                buf.append(c);
                break;
            }
        }
    }

    private boolean isDigit(char c)
    { return ('0' <= c) && (c <= '9'); }
    
    private boolean isLetter(char c)
    { return ('a' <= c) && (c <= 'z'); }
    
    private boolean isSpecialInitial(char c)
    { return "!$%&*/:<=>?^_~".indexOf(c) != -1; }
    
    private boolean isInitial(char c)
    {
        return isLetter(c)
            || isSpecialInitial(c);
    }

    private boolean isSubsequent(char c)
    {
        return isInitial(c)
            || isDigit(c)
            || ("+-.@".indexOf(c) != -1);
    }
    
        
    private Value parseNumOrSym(char initial)
        throws IOException, ParseException
    {
        StringBuffer buf = new StringBuffer();
        initial = Character.toLowerCase(initial);
        buf.append(initial);
        for (;;) {
            int c = _reader.read();
            if (c == EOF) {
                break;
            } else if (isDelimiter(c)) {
                _reader.unread(c);
                break;
            }
            buf.append(
                Character.toLowerCase((char)c)
            );
        }
        
        String str = buf.toString();
        
        if (str.equals("+")) {
            return ValueFactory.createSymbol("+");
        } else if (str.equals("-")) {
            return ValueFactory.createSymbol("-");
        } else if (str.equals("...")) {
            return ValueFactory.createSymbol("...");
        }

        checkNumber: {
            if (!isDigit(initial)) {
                if ((initial != '+') && (initial != '-')) {
                    break checkNumber;
                }
            }
            for (int i = 1; i < str.length(); i++) {
                if (!isDigit(str.charAt(i))) {
                    break checkNumber;
                }
            }
            try {
                return ValueFactory.createNumber(
                    Integer.parseInt(str)
                );
            }
            catch (NumberFormatException e) { }
        }
        
        if (!isInitial(initial)) {
            throw new ParseException(
                this,
                "invalid identifier"
            );
        }
        
        for (int i = 1; i < str.length(); i++) {
            if (!isSubsequent(str.charAt(i))) {
                throw new ParseException(
                    this,
                    "invalid identifier"
                );
            }
        }
        
        return ValueFactory.createSymbol(str);
    }

    private Value parseDatum()
        throws IOException, ParseException
    {
        int la1 = skipWSread();
        
        switch (la1) {
        case '#': {
            int la2 = _reader.read();

            switch (la2) {
            case 't':
                return ValueFactory.createTrue();

            case 'f':
                return ValueFactory.createFalse();
                
            case '\\':
                return parseChar();

            case '(':
                return parseVector(0);
                
            default:
                throw new ParseException(
                    this,
                    "'" + la2 + "' can't follow '#'"
                );
            }
        } // break;
                
        case '(':
            return parseList();
            
        case '"':
            return parseString();
        
        case '\'':
            return ValueFactory.createList(
                ValueFactory.createSymbol("quote"),
                parseDatum()
            );

        case '`':
            return ValueFactory.createList(
                ValueFactory.createSymbol("quasiquote"),
                parseDatum()
            );
            
        case ',': {
                int c = _reader.read();
                Symbol sym;

                if (c == '@') {
                    sym = ValueFactory.createSymbol("unquote-splicing");
                } else {
                    _reader.unread(c);
                    sym = ValueFactory.createSymbol("unquote");
                }

                return ValueFactory.createList(
                    sym,
                    parseDatum()
                );
            }
            

        case EOF:
            return EOF_VALUE;

        default:
            return parseNumOrSym((char)la1);
        }
    }


    public int readChar()
        throws ReadException
    {
        try {
            return _reader.read();
        }
        catch (IOException e) {
            throw new ReadException(this);
        }
    }
    
    public Value readScmChar()
        throws ReadException
    {
        int c = readChar();

        return
            (c == EOF)
            ? EOF_VALUE
            : SchemeChar.create((char)c);
    }
    
    public int peekChar()
        throws ReadException
    {
        try {
            int result = _reader.read();
            if (result != EOF) {
                _reader.unread(result);
            }
            return result;
        }
        catch (IOException e) {
            throw new ReadException(this);
        }
    }

    public Value peekScmChar()
        throws ReadException
    {
        int c = peekChar();

        return
            (c == EOF)
            ? EOF_VALUE
            : SchemeChar.create((char)c);
    }
    
    public boolean isReady()
    {
        try {
            return _reader.ready();
        }
        catch (IOException e) { }
        
        return false;
    }
}

