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
package de.masitec.mscheme.values.functions

import de.masitec.mscheme.exceptions.SchemeRuntimeError
import de.masitec.mscheme.exceptions.TypeError
import de.masitec.mscheme.values.IList

internal abstract class Reducer protected constructor(
    private val _initial: Any?
) {
    protected abstract fun combine(fst: Any?, snd: Any?): Any?

    fun reduceLeft(list: IList): Any? {
        if (list.isEmpty) {
            return _initial
        } else {
            var result = list.head

            var tail = list.tail
            while (!tail.isEmpty) {
                result = combine(result, tail.head)
                tail = tail.tail
            }

            return result
        }
    }

    fun foldLeft(list: IList): Any? {
        var result = _initial

        var tail = list
        while (!tail.isEmpty) {
            result = combine(result, tail.head)
            tail = tail.tail
        }

        return result
    }

    private fun reduceRightHelper(list: IList): Any? {
        val tail = list.tail

        return if (tail.isEmpty)
            list.head
        else
            combine(
                list.head,
                reduceRightHelper(tail)
            )
    }

    fun reduceRight(list: IList): Any? =
        if (list.isEmpty)
            _initial
        else
            reduceRightHelper(list)

    fun foldRight(list: IList): Any? =
        if (list.isEmpty)
            _initial
        else
            combine(
                list.head,
                foldRight(list.tail)
            )
}
