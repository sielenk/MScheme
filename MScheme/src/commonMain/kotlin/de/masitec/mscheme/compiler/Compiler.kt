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

/*
 * Created on 30.05.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package de.masitec.mscheme.compiler

import de.masitec.mscheme.environment.StaticEnvironment
import de.masitec.mscheme.exceptions.CantCompileException
import de.masitec.mscheme.syntax.ITranslator
import de.masitec.mscheme.syntax.ProcedureCall
import de.masitec.mscheme.values.ICompileable
import de.masitec.mscheme.values.ValueTraits


class Compiler(private val _env: StaticEnvironment) {
    fun getForceable(obj: Any?): Any? =
        if (ValueTraits.isScmVector(obj)) {
            throw CantCompileException(obj)
        } else if (ValueTraits.isSymbol(obj)) {
            _env.setStateClosed()
            _env.getDelayedReferenceFor(obj as String)
        } else if (obj is ICompileable) {
            obj.getForceable(_env)
        } else {
            _env.setStateClosed()
            ValueTraits.getConst(obj)
        }

    fun getTranslator(obj: Any?): ITranslator {
        if (obj is String) {
            val result = _env.getSyntaxFor(obj)

            if (result != null) {
                return result
            }
        }

        return ProcedureCall.create(obj)
    }

    fun compile(compilee: Any?): Any? =
        force(getForceable(compilee))

    companion object {
        fun force(o: Any?): Any? =
            if (o is IForceable) o.force() else o
    }
}
