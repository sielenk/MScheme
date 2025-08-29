/*
 * The empty list singleton. Copyright (C) 2001 Marvin H. Sielenkemper
 *
 * This file is part of MScheme.
 *
 * MScheme is free software; you can redistribute it and/or modify it under the
 * terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 2 of the License, or (at your option) any later
 * version.
 *
 * MScheme is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
 * A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * mscheme; see the file COPYING. If not, write to the Free Software Foundation,
 * Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */
package mscheme.values

import mscheme.environment.StaticEnvironment
import mscheme.exceptions.CantCompileException
import mscheme.exceptions.ListExpected
import mscheme.exceptions.PairExpected
import java.io.IOException
import java.io.Writer

object Empty : IList, IOutputable, ICompileable {
    override val isValid: Boolean
        // implementation of List
        get() = true

    override fun validate(): IList =
        this

    override val isEmpty: Boolean
        get() = true

    override fun getCopy(): IList =
        this

    override val length: Int
        get() = 0

    override fun getReversed(): IList =
        this

    override val head: Any?
        get() {
            throw PairExpected(this)
        }

    @get:Throws(PairExpected::class)
    override val tail: IList
        get() {
            throw PairExpected(this)
        }

    override fun getForceable(compilationEnv: StaticEnvironment): Any? {
        throw CantCompileException(this)
    }

    override fun getCompiledArray(compilationEnv: StaticEnvironment): Array<Any?> =
        ARRAY

    override fun getCompiledArray(compilationEnv: StaticEnvironment, index: Int): Array<Any?> =
        arrayOfNulls(index)

    override fun getArray(): Array<Any?> =
        ARRAY

    override fun getArray(index: Int): Array<Any?> =
        arrayOfNulls(index)

    override fun outputOn(destination: Writer, doWrite: Boolean) {
        destination.write("()")
    }

    private val ARRAY = arrayOf<Any?>()
}
