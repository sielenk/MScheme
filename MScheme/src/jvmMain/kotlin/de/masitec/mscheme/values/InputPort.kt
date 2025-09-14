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
package de.masitec.mscheme.values

import de.masitec.mscheme.exceptions.CloseException
import de.masitec.mscheme.exceptions.ParseException
import de.masitec.mscheme.exceptions.ReadException
import de.masitec.mscheme.util.Reader
import de.masitec.mscheme.util.Writer
import de.masitec.mscheme.util.createReader


internal object EofValue : IOutputable {
    override fun outputOn(destination: Writer, doWrite: Boolean) {
        destination.write("#[eof]")
    }
}


class InputPort private constructor(
    private val _reader: Reader
) : IOutputable, Port() {
    // specialisation of Port

    override fun outputOn(destination: Writer, doWrite: Boolean) {
        destination.write("#[input port]")
    }


    fun close() {
        try {
            _reader.close()
        } catch (e: Exception) {
            throw CloseException(this)
        }
    }

    // input port
    fun read(): Any? =
        try {
            parseDatum()
        } catch (e: Exception) {
            throw ReadException(this)
        }


    private fun isWhitespace(c: Int): Boolean =
        (c == ' '.code) || (c == '\t'.code) || (c == '\n'.code)

    private fun isDelimiter(c: Int): Boolean =
        isWhitespace(c)
                || (c == '('.code)
                || (c == ')'.code)
                || (c == '['.code)
                || (c == ']'.code)
                || (c == '"'.code) // "
                || (c == ';'.code)

    private fun skipWSread(): Int {
        var inComment = false

        while (true) {
            val c = _reader.read()

            if (inComment) {
                when (c) {
                    '\n'.code -> inComment = false
                    EOF -> return c
                }
            } else {
                if (c == ';'.code) {
                    inComment = true
                } else if (!isWhitespace(c)) {
                    return c
                }
            }
        }
    }

    private fun readNoEof(): Char {
        val c = _reader.read()

        if (c == EOF) {
            throw ParseException(
                this,
                "unexpected EOF"
            )
        }

        return c.toChar()
    }

    private fun parseChar(): Char {
        var c = readNoEof().code

        if ((c == 's'.code) || (c == 'n'.code)) {
            val charName = StringBuilder()

            do {
                charName.append(c.toChar())
                c = _reader.read()
            } while ((c != EOF) && !isDelimiter(c))

            if (c != EOF) {
                _reader.unread(c)
            }

            val name = charName.toString()

            return when (name) {
                "s" -> ValueTraits.toScmChar('s')
                "n" -> ValueTraits.toScmChar('n')
                "space" -> ValueTraits.toScmChar(' ')
                "newline" -> ValueTraits.toScmChar('\n')
                else -> throw ParseException(
                    this,
                    "invalid character name '" + name + "'"
                )
            }
        } else {
            return ValueTraits.toScmChar(c.toChar())
        }
    }

    private fun parseVector(index: Int): Array<Any?> {
        val la = skipWSread()

        if (la == ')'.code) {
            return arrayOfNulls(index)
        } else if (la == EOF) {
            throw ParseException(
                this,
                "unexpected EOF"
            )
        } else {
            _reader.unread(la)

            val head = parseDatum()
            val result = parseVector(index + 1)

            result[index] = head

            return result
        }
    }

    private fun parseList(closeToken: Char): Any {
        var la = skipWSread()

        if (la == closeToken.code) {
            return ListFactory.createConst()
        }

        if (la == EOF) {
            throw ParseException(
                this,
                "unexpected EOF"
            )
        } else {
            _reader.unread(la)
            val head = parseDatum()

            la = skipWSread()
            if (la == '.'.code) {
                val result = ListFactory.createConstPair(
                    head,
                    parseDatum()
                )

                if (skipWSread() != ')'.code) {
                    throw ParseException(
                        this,
                        "expected ')'"
                    )
                }

                return result
            } else {
                _reader.unread(la)
                return ListFactory.createConstPair(
                    head,
                    parseList(closeToken)
                )
            }
        }
    }

    private fun parseString(): ScmString {
        val buf = StringBuilder()

        while (true) {
            val c = readNoEof()

            when (c) {
                '"' -> return ScmString.createConst(buf.toString())
                '\\' -> {
                    val esc = readNoEof()
                    buf.append(
                        when (esc) {
                            'n' -> '\n'
                            '\\' -> '\\'
                            '"' -> '"'
                            else -> esc
                        }
                    )
                }

                else -> buf.append(c)
            }
        }
    }

    private fun isDigit(c: Char): Boolean =
        ('0' <= c) && (c <= '9')

    private fun isLetter(c: Char): Boolean =
        ('a' <= c) && (c <= 'z')

    private fun isSpecialInitial(c: Char): Boolean =
        "!$%&*/:<=>?^_~".indexOf(c) != -1

    private fun isInitial(c: Char): Boolean =
        isLetter(c) || isSpecialInitial(c)

    private fun isSubsequent(c: Char): Boolean =
        isInitial(c) || isDigit(c) || ("+-.@".indexOf(c) != -1)


    private fun parseNumOrSym(initial: Char): Any? {
        var initial = initial
        val buf = StringBuilder()

        initial = initial.lowercaseChar()
        buf.append(initial)
        while (true) {
            val c = _reader.read()

            if (c == EOF) {
                break
            } else if (isDelimiter(c)) {
                _reader.unread(c)
                break
            }

            buf.append(
                c.toChar().lowercaseChar()
            )
        }

        val str = buf.toString()

        when (str) {
            "+" -> return _plus // "+";
            "-" -> return _minus // "-";
            "..." -> return _ellipsis // "...";
        }

        do {
            if (!isDigit(initial)) {
                if ((initial != '+') && (initial != '-')) {
                    break
                }
            }
            for (i in 1..<str.length) {
                if (!isDigit(str.get(i))) {
                    break
                }
            }
            try {
                return ScmNumber.create(str)
            } catch (e: NumberFormatException) {
            }
        } while (false)

        if (!isInitial(initial)) {
            throw ParseException(
                this,
                "invalid identifier"
            )
        }

        for (i in 1..<str.length) {
            if (!isSubsequent(str.get(i))) {
                throw ParseException(
                    this,
                    "invalid identifier"
                )
            }
        }

        return str.intern()
    }

    private fun parseDatum(): Any? {
        val la1 = skipWSread()

        when (la1) {
            '#'.code -> {
                val la2 = _reader.read()

                return when (la2) {
                    't'.code -> ValueTraits.TRUE
                    'f'.code -> ValueTraits.FALSE
                    '\\'.code -> parseChar()
                    '('.code -> ScmVector.createConst(parseVector(0))
                    else -> throw ParseException(
                        this,
                        "'$la2' can't follow '#'"
                    )
                }
            } // break;
            '('.code -> return parseList(')')
            '['.code -> return parseList(']')
            '"'.code -> return parseString()
            '\''.code -> return ListFactory.createConst(
                "quote",
                parseDatum()
            )

            '`'.code -> return ListFactory.createConst(
                "quasiquote",
                parseDatum()
            )

            ','.code -> {
                val la2 = _reader.read()
                val sym: String?

                if (la2 == '@'.code) {
                    sym = "unquote-splicing"
                } else {
                    _reader.unread(la2)
                    sym = "unquote"
                }

                return ListFactory.createConst(
                    sym,
                    parseDatum()
                )
            }

            EOF -> return EOF_VALUE
            else -> return parseNumOrSym(la1.toChar())
        }
    }


    fun readChar(): Int {
        try {
            return _reader.read()
        } catch (e: Exception) {
            throw ReadException(this)
        }
    }

    fun readScmChar(): Any? {
        val c = readChar()

        return if (c == EOF)
            EOF_VALUE
        else
            ValueTraits.toScmChar(c.toChar())
    }

    fun peekChar(): Int {
        try {
            val result = _reader.read()

            if (result != EOF) {
                _reader.unread(result)
            }

            return result
        } catch (e: Exception) {
            throw ReadException(this)
        }
    }

    fun peekScmChar(): Any? {
        val c = peekChar()

        return if (c == EOF)
            EOF_VALUE
        else
            ValueTraits.toScmChar(c.toChar())
    }

    val isReady: Boolean
        get() = _reader.ready()


    companion object {
        const val EOF: Int = -1

        val EOF_VALUE: Any = EofValue

        private val _plus: Any = "+"
        private val _minus: Any = "-"
        private val _ellipsis: Any = "..."

        fun create(reader: Reader): InputPort =
            InputPort(reader)

        fun create(filename: ScmString): InputPort =
            create(filename.javaString)

        fun create(filename: String): InputPort =
            create(createReader(filename))
    }
}
