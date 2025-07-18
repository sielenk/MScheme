/* The implementation of Scheme's output ports.
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
package mscheme.values

import mscheme.exceptions.CloseException
import mscheme.exceptions.OpenException
import mscheme.exceptions.WriteException
import java.io.FileWriter
import java.io.IOException
import java.io.Writer

class OutputPort private constructor(private val _writer: Writer) : Port() {
    // specialisation of Port

    @Throws(IOException::class)
    fun writeOn(destination: Writer) {
        destination.write("#[output port]")
    }

    fun toOutputPort(): OutputPort =
        this


    @Throws(CloseException::class)
    fun close() {
        try {
            _writer.close()
        } catch (e: IOException) {
            throw CloseException(this)
        }
    }

    // output port
    @Throws(WriteException::class)
    fun writeChar(c: Char) {
        try {
            _writer.write(c.code)
            _writer.flush()
        } catch (e: IOException) {
            throw WriteException(this)
        }
    }

    @Throws(WriteException::class)
    fun writeScmChar(c: Char) {
        writeChar(c)
    }

    @Throws(WriteException::class)
    fun write(datum: Any?) {
        try {
            ValueTraits.write(_writer, datum)
            _writer.flush()
        } catch (e: IOException) {
            throw WriteException(this)
        }
    }

    @Throws(WriteException::class)
    fun display(datum: Any?) {
        try {
            ValueTraits.display(_writer, datum)
            _writer.flush()
        } catch (e: IOException) {
            throw WriteException(this)
        }
    }

    companion object {
        @JvmStatic
        fun create(writer: Writer): OutputPort =
            OutputPort(writer)

        @JvmStatic
        @Throws(OpenException::class)
        fun create(filename: ScmString): OutputPort =
            create(filename.javaString)

        @Throws(OpenException::class)
        fun create(filename: String): OutputPort =
            try {
                create(FileWriter(filename))
            } catch (e: IOException) {
                throw OpenException(
                    ScmString.create(filename)
                )
            }
    }
}
