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

import de.masitec.mscheme.environment.StaticEnvironment
import de.masitec.mscheme.exceptions.PairExpected

interface IList {
    val isValid: Boolean

    fun validate(): IList

    val isEmpty: Boolean

    val length: Int

    fun getReversed(): IList

    fun getCopy(): IList

    @get:Throws(PairExpected::class)
    val head: Any?

    @get:Throws(PairExpected::class)
    val tail: IList

    fun getCompiledArray(compilationEnv: StaticEnvironment): Array<Any?>

    fun getCompiledArray(compilationEnv: StaticEnvironment, index: Int): Array<Any?>

    fun getArray(): Array<Any?>

    fun getArray(index: Int): Array<Any?>
}
