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

package de.masitec.mscheme.util


private class JsStringWriter : Writer {
    private val sb = StringBuilder()

    override fun write(i: Int) {
        sb.append(i.toChar())
    }

    override fun write(s: String) {
        sb.append(s)
    }

    override fun flush() {}
    override fun close() {}
    override fun toString(): String =
        sb.toString()
}

private class JsStdOutWriter : Writer {
    private val sb = StringBuilder()

    override fun write(i: Int) {
        sb.append(i.toChar())
    }

    override fun write(s: String) {
        sb.append(s)
    }

    override fun flush() {
        print(sb)
        sb.clear()
    }

    override fun close() {
        flush()
    }
}

actual fun createStdOutWriter(): Writer =
    JsStdOutWriter()

actual fun createWriter(fileName: String): Writer =
    error("file IO not supported on JS")

actual fun writeToString(f: (Writer) -> Unit): String =
    JsStringWriter().also {
        f(it)
    }.toString()
