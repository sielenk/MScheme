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
package de.masitec.mscheme.compiler

import de.masitec.mscheme.environment.StaticEnvironment
import de.masitec.mscheme.exceptions.CantCompileException
import de.masitec.mscheme.syntax.ITranslator
import de.masitec.mscheme.syntax.ProcedureCall
import de.masitec.mscheme.values.ICompileable
import de.masitec.mscheme.values.ValueTraits

/**
 * The syntactic position of the expression being compiled
 * and the static environment it is compiled in.
 */
data class CompilationContext(
    val kind: Kind,
    val env: StaticEnvironment
) {
    enum class Kind {
        /** A top level form, definitions and expressions may be mixed. */
        TOP_LEVEL,

        /** The start of a body, before the first expression. */
        BODY,

        /** An expression, definitions are not allowed. */
        EXPRESSION,

        /**
         * The value of an internal definition. Like EXPRESSION, but
         * variables of the same body may not be referenced directly.
         */
        DEFINITION_VALUE
    }

    val definitionsAllowed: Boolean
        get() = kind == Kind.TOP_LEVEL || kind == Kind.BODY

    val restricted: Boolean
        get() = kind == Kind.DEFINITION_VALUE

    /**
     * The context of a subexpression. The restriction of a
     * definition value holds until a new body starts.
     */
    fun expression(): CompilationContext =
        if (kind == Kind.DEFINITION_VALUE) this else copy(kind = Kind.EXPRESSION)

    fun definitionValue(): CompilationContext =
        copy(kind = Kind.DEFINITION_VALUE)

    /**
     * The context of a new body in the given environment.
     */
    fun body(bodyEnv: StaticEnvironment): CompilationContext =
        CompilationContext(Kind.BODY, bodyEnv)

    fun getForceable(obj: Any?): Any? =
        if (ValueTraits.isScmVector(obj)) {
            throw CantCompileException(obj)
        } else if (ValueTraits.isSymbol(obj)) {
            env.getDelayedReferenceFor(
                ValueTraits.toSymbol(obj), restricted
            )
        } else if (obj is ICompileable) {
            obj.getForceable(this)
        } else {
            ValueTraits.getConst(obj)
        }

    fun getTranslator(obj: Any?): ITranslator {
        if (ValueTraits.isSymbol(obj)) {
            val result = env.getSyntaxFor(ValueTraits.toSymbol(obj))

            if (result != null) {
                return result
            }
        }

        return ProcedureCall.create(obj)
    }
}
