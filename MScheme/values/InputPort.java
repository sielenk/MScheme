/* The implementation of Scheme's input ports.
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

package MScheme.values;

import java.io.FileReader;
import java.io.IOException;
import java.io.PushbackReader;
import java.io.Reader;
import java.io.Writer;

import MScheme.Value;

import MScheme.exceptions.CloseException;
import MScheme.exceptions.OpenException;
import MScheme.exceptions.ParseException;
import MScheme.exceptions.ReadException;

class EofValue
    extends ValueDefaultImplementations
{
    public final static String id
        = "$Id$";


    public final static EofValue INSTANCE = new EofValue();

    private EofValue()
    { }

    public void write(Writer dest)
        throws IOException
    {
        dest.write("#[eof]");
    }
}


public class InputPort
    extends Port
{
    public final static String id
        = "$Id$";


    public final static int   EOF       = -1;
    public final static Value EOF_VALUE = EofValue.INSTANCE;


    private final PushbackReader _reader;

    private InputPort(PushbackReader reader)
    {
        _reader = reader;
    }


    public static InputPort create(Reader reader)
    {
        return new InputPort(
                   reader instanceof PushbackReader
                   ? (PushbackReader)reader
                   : new PushbackReader(reader)
               );
    }

    public static InputPort create(ScmString filename)
        throws OpenException
    {
        return create(filename.getJavaString());
    }

    public static InputPort create(String filename)
        throws OpenException
    {
        try
        {
            return create(new FileReader(filename));
        }
        catch (IOException e)
        {
            throw new OpenException(
                      ScmString.create(filename)
                  );
        }
    }


    // specialisation of Port

    public void write(Writer destination)
        throws IOException
    {
        destination.write("#[input port]");
    }

    public InputPort toInputPort()
    {
        return this;
    }


    public void close()
        throws CloseException
    {
        try
        {
            _reader.close();
        }
        catch (IOException e)
        {
            throw new CloseException(this);
        }
    }


    // input port

    public Value read()
        throws ReadException, ParseException
    {
        try
        {
            return parseDatum();
        }
        catch (IOException e)
        {
            throw new ReadException(this);
        }
    }


    private boolean isWhitespace(int c)
    {
        return (c == ' ') || (c == '\t') || (c == '\n');
    }

    private boolean isDelimiter(int c)
    {
        return isWhitespace(c)
               || (c == '(')
               || (c == ')')
               || (c == '[')
               || (c == ']')
               || (c == '"')    // "
               || (c == ';');
    }

    private int skipWSread()
        throws IOException
    {
        boolean inComment = false;

        for (;;)
        {
            int c = _reader.read();

            if (inComment)
            {
                switch (c)
                {
                case '\n':
                    inComment = false;
                    break;

                case EOF:
                    return c;
                }
            }
            else
            {
                if (c == ';')
                {
                    inComment = true;
                }
                else if (!isWhitespace(c))
                {
                    return c;
                }
            }
        }
    }

    private char readNoEof()
        throws IOException, ParseException
    {
        int c = _reader.read();
        if (c == EOF)
        {
            throw new ParseException(
                      this,
                      "unexpected EOF"
                  );
        }
        return (char)c;
    }

    private ScmChar parseChar()
        throws IOException, ParseException
    {
        int c = readNoEof();

        if ((c == 's') || (c == 'n'))
        {
            StringBuffer charName = new StringBuffer();

            do
            {
                charName.append((char)c);
                c = _reader.read();
            }
            while ((c != EOF) && !isDelimiter(c));

            if (c != EOF)
            {
                _reader.unread(c);
            }

            String name = charName.toString();

            if (name.equals("s"))
            {
                return ScmChar.create('s');
            }
            else if (name.equals("n"))
            {
                return ScmChar.create('n');
            }
            else if (name.equals("space"))
            {
                return ScmChar.create(' ');
            }
            else if (name.equals("newline"))
            {
                return ScmChar.create('\n');
            }
            else
            {
                throw new ParseException(
                          this,
                          "invalid character name '" + name + "'"
                      );
            }
        }
        else
        {
            return ScmChar.create((char)c);
        }
    }

    private Value[] parseVector(int index)
        throws IOException, ParseException
    {
        int la = skipWSread();

        if (la == ')')
        {
            return new Value[index];
        }
        else if (la == EOF)
        {
            throw new ParseException(
                      this,
                      "unexpected EOF"
                  );
        }
        else
        {
            _reader.unread(la);

            Value   head   = parseDatum();
            Value[] result = parseVector(index + 1);

            result[index] = head;

            return result;
        }
    }

    private Value parseList(char closeToken)
        throws IOException, ParseException
    {
        int la = skipWSread();

        if (la == closeToken)
        {
            return Empty.create();
        }
        if (la == EOF)
        {
            throw new ParseException(
                      this,
                      "unexpected EOF"
                  );
        }
        else
        {
            _reader.unread(la);
            Value head = parseDatum();

            la = skipWSread();
            if (la == '.')
            {
                Pair result = ListFactory.createConstPair(
                                  head,
                                  parseDatum()
                              );

                if (skipWSread() != ')')
                {
                    throw new ParseException(
                              this,
                              "expected ')'"
                          );
                }

                return result;
            }
            else
            {
                _reader.unread(la);
                return ListFactory.createConstPair(
                           head,
                           parseList(closeToken)
                       );
            }
        }
    }

    private ScmString parseString()
        throws IOException, ParseException
    {
        StringBuffer buf = new StringBuffer();

        for (;;)
        {
            char c = readNoEof();

            switch (c)
            {
            case '"':  // "
                return ScmString.createConst(
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
    {
        return ('0' <= c) && (c <= '9');
    }

    private boolean isLetter(char c)
    {
        return ('a' <= c) && (c <= 'z');
    }

    private boolean isSpecialInitial(char c)
    {
        return "!$%&*/:<=>?^_~".indexOf(c) != -1;
    }

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
        for (;;)
        {
            int c = _reader.read();
            if (c == EOF)
            {
                break;
            }
            else if (isDelimiter(c))
            {
                _reader.unread(c);
                break;
            }
            buf.append(
                Character.toLowerCase((char)c)
            );
        }

        String str = buf.toString();

        if (str.equals("+"))
        {
            return Symbol.create("+");
        }
        else if (str.equals("-"))
        {
            return Symbol.create("-");
        }
        else if (str.equals("..."))
        {
            return Symbol.create("...");
        }

checkNumber:
        {
            if (!isDigit(initial))
            {
                if ((initial != '+') && (initial != '-'))
                {
                    break checkNumber;
                }
            }
            for (int i = 1; i < str.length(); i++)
            {
                if (!isDigit(str.charAt(i)))
                {
                    break checkNumber;
                }
            }
            try
            {
                return ScmNumber.create(str);
            }
            catch (NumberFormatException e)
            { }
        }

        if (!isInitial(initial))
        {
            throw new ParseException(
                      this,
                      "invalid identifier"
                  );
        }

        for (int i = 1; i < str.length(); i++)
        {
            if (!isSubsequent(str.charAt(i)))
            {
                throw new ParseException(
                          this,
                          "invalid identifier"
                      );
            }
        }

        return Symbol.create(str);
    }

    private Value parseDatum()
        throws IOException, ParseException
    {
        int la1 = skipWSread();

        switch (la1)
        {
        case '#':
            {
                int la2 = _reader.read();

                switch (la2)
                {
                case 't':
                    return ScmBoolean.createTrue();

                case 'f':
                    return ScmBoolean.createFalse();

                case '\\':
                    return parseChar();

                case '(':
                    return ScmVector.createConst(parseVector(0));

                default:
                    throw new ParseException(
                              this,
                              "'" + la2 + "' can't follow '#'"
                          );
                }
            } // break;

        case '(':
            return parseList(')');

        case '[':
            return parseList(']');

        case '"':  // "
            return parseString();

        case '\'':
            return ListFactory.create(
                       Symbol.create("quote"),
                       parseDatum()
                   );

        case '`':
            return ListFactory.create(
                       Symbol.create("quasiquote"),
                       parseDatum()
                   );

        case ',':
            {
                int la2 = _reader.read();
                Symbol sym;

                if (la2 == '@')
                {
                    sym = Symbol.create("unquote-splicing");
                }
                else
                {
                    _reader.unread(la2);
                    sym = Symbol.create("unquote");
                }

                return ListFactory.create(
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
        try
        {
            return _reader.read();
        }
        catch (IOException e)
        {
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
            : ScmChar.create((char)c);
    }

    public int peekChar()
        throws ReadException
    {
        try
        {
            int result = _reader.read();
            if (result != EOF)
            {
                _reader.unread(result);
            }
            return result;
        }
        catch (IOException e)
        {
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
            : ScmChar.create((char)c);
    }

    public boolean isReady()
    {
        try
        {
            return _reader.ready();
        }
        catch (IOException e)
        { }

        return false;
    }
}
