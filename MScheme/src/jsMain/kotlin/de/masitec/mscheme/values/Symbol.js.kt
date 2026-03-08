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

package de.masitec.mscheme.values

private val symbolTable =
    mutableMapOf<String, Symbol>()

private class JsSymbol(private val str: String) : Symbol {
    override fun hashCode(): Int =
        str.hashCode()

    override fun eq(other: Any?): Boolean =
        when (other) {
            is JsSymbol -> this === other
            is String -> str == other
            else -> false
        }

    override fun eqv(other: Any?): Boolean =
        eq(other)

    override fun equals(other: Any?): Boolean =
        eq(other)

    override fun toString(): String =
        str

    override fun compareTo(other: Symbol): Int =
        str.compareTo(other.toString())
}

internal actual fun Symbol(str: String): Symbol =
    symbolTable.getOrPut(str) { JsSymbol(str) }
