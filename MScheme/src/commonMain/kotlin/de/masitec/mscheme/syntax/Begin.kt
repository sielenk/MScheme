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
package de.masitec.mscheme.syntax

import de.masitec.mscheme.code.Sequence
import de.masitec.mscheme.compiler.CompilationContext
import de.masitec.mscheme.compiler.Definition
import de.masitec.mscheme.compiler.IForceable
import de.masitec.mscheme.util.Arity
import de.masitec.mscheme.values.IList

internal class Begin private constructor(
    private val _tag: SequenceTags
) : CheckedTranslator(
    Arity.atLeast(if (_tag == SequenceTags.BEGIN) 1 else 0)
) {
    override fun checkedTranslate(
        context: CompilationContext,
        arguments: IList
    ): Any? {
        if (_tag != SequenceTags.BEGIN) {
            return Sequence.create(
                _tag,
                arguments.getCompiledArray(context.expression())
            )
        }

        // a begin in a definition context is spliced into
        // the surrounding body or top level
        val compiled = arguments.getCompiledBody(context)
        val result = Sequence.create(_tag, compiled)

        return if (context.definitionsAllowed
            && result !is Definition
            && compiled.all { it is Definition }
        )
            Definition(result as IForceable)
        else
            result
    }


    companion object {
        val INSTANCE_BEGIN: ITranslator =
            Begin(SequenceTags.BEGIN)

        val INSTANCE_AND: ITranslator =
            Begin(SequenceTags.AND)

        val INSTANCE_OR: ITranslator =
            Begin(SequenceTags.OR)
    }
}
