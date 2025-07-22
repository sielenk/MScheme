/*
 * The translation function for Scheme's 'begin'. Copyright (C) 2001 Marvin H.
 * Sielenkemper
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
 * MScheme; see the file COPYING. If not, write to the Free Software Foundation,
 * Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */
package mscheme.syntax

import mscheme.code.Sequence.Companion.create
import mscheme.environment.StaticEnvironment
import mscheme.exceptions.SchemeException
import mscheme.util.Arity.Companion.atLeast
import mscheme.values.IList

internal class Begin private constructor(
    private val _tag: Int
) : CheckedTranslator(
    atLeast(if (_tag == ISequenceTags.TAG_BEGIN) 1 else 0)
), ISequenceTags {
    override fun preTranslate(compilationEnv: StaticEnvironment) {
        if (_tag != ISequenceTags.TAG_BEGIN) {
            super.preTranslate(compilationEnv)
        }
    }

    @Throws(SchemeException::class, InterruptedException::class)
    override fun checkedTranslate(
        compilationEnv: StaticEnvironment, arguments: IList
    ): Any? =
        create(_tag, arguments.getCompiledArray(compilationEnv))


    companion object {
        @JvmField
        val INSTANCE_BEGIN: ITranslator =
            Begin(ISequenceTags.TAG_BEGIN)

        @JvmField
        val INSTANCE_AND: ITranslator =
            Begin(ISequenceTags.TAG_AND)

        @JvmField
        val INSTANCE_OR: ITranslator =
            Begin(ISequenceTags.TAG_OR)
    }
}