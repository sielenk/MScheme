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

package mscheme.values;

import java.io.FileReader;
import java.io.IOException;
import java.io.InterruptedIOException;
import java.io.PushbackReader;
import java.io.Reader;
import java.io.Writer;

import mscheme.exceptions.CloseException;
import mscheme.exceptions.OpenException;
import mscheme.exceptions.ParseException;
import mscheme.exceptions.ReadException;

class EofValue
	implements IOutputable
{
    public final static String CVS_ID
        = "$Id$";


    public final static EofValue INSTANCE = new EofValue();

    private EofValue()
    { }

    public void outputOn(Writer dest, boolean ignored)
        throws IOException
    {
        dest.write("#[eof]");
    }
}


public class InputPort
    extends Port
{
    public final static String CVS_ID
        = "$Id$";


    public final static int    EOF       = -1;
    public final static Object EOF_VALUE = EofValue.INSTANCE;

    private static final Object _plus = "+".intern();
    private static final Object _minus = "-".intern();
    private static final Object _ellipsis = "...".intern();

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

    public void writeOn(Writer destination)
        throws IOException
    {
        destination.write("#[input port]");
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

    public Object read()
        throws ReadException, ParseException, InterruptedException
    {
        try
        {
            return parseDatum();
        }
        catch (InterruptedIOException e)
        {
            throw new InterruptedException(e.getMessage());
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

    private Character parseChar()
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
                return ValueTraits.toScmChar('s');
            }
            else if (name.equals("n"))
            {
                return ValueTraits.toScmChar('n');
            }
            else if (name.equals("space"))
            {
                return ValueTraits.toScmChar(' ');
            }
            else if (name.equals("newline"))
            {
                return ValueTraits.toScmChar('\n');
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
            return ValueTraits.toScmChar((char)c);
        }
    }

    private Object[] parseVector(int index)
        throws IOException, ParseException
    {
        int la = skipWSread();

        if (la == ')')
        {
            return new Object[index];
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

            Object   head   = parseDatum();
            Object[] result = parseVector(index + 1);

            result[index] = head;

            return result;
        }
    }

    private Object parseList(char closeToken)
        throws IOException, ParseException
    {
        int la = skipWSread();

        if (la == closeToken)
        {
            return ListFactory.createConst();
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
            Object head = parseDatum();

            la = skipWSread();
            if (la == '.')
            {
                IPair result = ListFactory.createConstPair(
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
                return ScmString.createConst(buf.toString());

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


    private Object parseNumOrSym(char initial)
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
            return _plus; // "+";
        }
        else if (str.equals("-"))
        {
            return _minus; // "-";
        }
        else if (str.equals("..."))
        {
            return _ellipsis; // "...";
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

        return str.intern();
    }

    private Object parseDatum()
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
                    return ValueTraits.TRUE;

                case 'f':
                    return ValueTraits.FALSE;

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
            return ListFactory.createConst(
                       "quote",
                       parseDatum()
                   );

        case '`':
            return ListFactory.createConst(
                       "quasiquote",
                       parseDatum()
                   );

        case ',':
            {
                int la2 = _reader.read();
                String sym;

                if (la2 == '@')
                {
                    sym = "unquote-splicing";
                }
                else
                {
                    _reader.unread(la2);
                    sym = "unquote";
                }

                return ListFactory.createConst(
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

    public Object readScmChar()
        throws ReadException
    {
        int c = readChar();

        return
            (c == EOF)
            ? EOF_VALUE
            : ValueTraits.toScmChar((char)c);
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

    public Object peekScmChar()
        throws ReadException
    {
        int c = peekChar();

        return
            (c == EOF)
            ? EOF_VALUE
            : ValueTraits.toScmChar((char)c);
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
