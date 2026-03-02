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
import de.masitec.mscheme.exceptions.OpenException
import de.masitec.mscheme.exceptions.WriteException
import de.masitec.mscheme.util.Writer
import de.masitec.mscheme.util.createWriter


class OutputPort private constructor(
    private val _writer: Writer
) : IOutputable, Port() {
    // specialisation of Port

    override fun outputOn(destination: Writer, doWrite: Boolean) {
        destination.write("#[output port]")
    }

    fun toOutputPort(): OutputPort =
        this


    fun close() {
        try {
            _writer.close()
        } catch (e: Exception) {
            throw CloseException(this)
        }
    }

    // output port
    fun writeChar(c: Char) {
        try {
            _writer.write(c.code)
            _writer.flush()
        } catch (e: Exception) {
            throw WriteException(this)
        }
    }

    fun writeScmChar(c: Char) {
        writeChar(c)
    }

    fun write(datum: Any?) {
        try {
            ValueTraits.write(_writer, datum)
            _writer.flush()
        } catch (e: Exception) {
            throw WriteException(this)
        }
    }

    fun display(datum: Any?) {
        try {
            ValueTraits.display(_writer, datum)
            _writer.flush()
        } catch (e: Exception) {
            throw WriteException(this)
        }
    }

    companion object {
        fun create(writer: Writer): OutputPort =
            OutputPort(writer)

        fun create(filename: ScmString): OutputPort =
            create(filename.javaString)

        fun create(filename: String): OutputPort =
            try {
                create(createWriter(filename))
            } catch (e: Exception) {
                throw OpenException(
                    ScmString.create(filename)
                )
            }
    }
}
