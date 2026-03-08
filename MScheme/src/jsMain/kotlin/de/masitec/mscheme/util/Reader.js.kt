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


private class JsStringReader(private val str: String) : Reader {
    private var pos = 0
    private var pushback = -1

    override fun ready(): Boolean = true

    override fun read(): Int {
        if (pushback != -1) {
            val ch = pushback
            pushback = -1
            return ch
        }
        return if (pos < str.length) str[pos++].code else -1
    }

    override fun unread(ch: Int) {
        pushback = ch
    }

    override fun close() {}
}

private object JsEofReader : Reader {
    override fun ready(): Boolean = false
    override fun read(): Int = -1
    override fun unread(ch: Int) {}
    override fun close() {}
}

actual fun createStdInReader(): Reader = JsEofReader

actual fun createReader(fileName: String): Reader =
    error("file IO not supported on JS")

actual fun createStringReader(string: String): Reader =
    JsStringReader(string)
