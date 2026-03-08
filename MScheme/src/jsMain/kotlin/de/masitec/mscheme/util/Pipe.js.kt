/*
 * Copyright (C) 2026  Marvin H. Sielenkemper
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

private class PipeWriter(private val buffer: ArrayDeque<Int>) : Writer {
    override fun write(i: Int) { buffer.addLast(i) }
    override fun write(s: String) { s.forEach { buffer.addLast(it.code) } }
    override fun flush() {}
    override fun close() {}
}

private class PipeReader(private val buffer: ArrayDeque<Int>) : Reader {
    private var pushback = -1

    override fun ready(): Boolean = pushback != -1 || buffer.isNotEmpty()

    override fun read(): Int {
        if (pushback != -1) {
            val ch = pushback
            pushback = -1
            return ch
        }
        return if (buffer.isNotEmpty()) buffer.removeFirst() else -1
    }

    override fun unread(ch: Int) { pushback = ch }

    override fun close() {}
}

actual fun createReaderWriterPipe(): Pair<Reader, Writer> {
    val buffer = ArrayDeque<Int>()

    return PipeReader(buffer) to PipeWriter(buffer)
}
