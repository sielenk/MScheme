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

package mscheme.values.functions

import mscheme.values.Function
import mscheme.values.IList
import kotlin.reflect.KFunction
import kotlin.reflect.KProperty0
import kotlin.reflect.KVisibility


fun getBuiltins(): Sequence<Pair<String, Any>> = sequence {
    val members = Builtins::class.members
        .filter { it.visibility == KVisibility.PUBLIC }

    for (member in members) {
        when (member) {
            is KProperty0<*> -> {
                val function = member.get() as? Function

                if (function != null) {
                    yield(parseName(member.name) to function)
                }
            }

            is KFunction<*> -> {
                val parameters = member.parameters
                val hasListParam = parameters.size == 1 &&
                        parameters[0].type.let {
                            /*!it.isMarkedNullable &&*/ it.classifier == IList::class
                        }
                val hasAnyParams = parameters.map { it.type }.all {
                    /*it.isMarkedNullable &&*/ it.classifier == Any::class
                }
                val hasAnyResult = member.returnType.let {
                    /*it.isMarkedNullable &&*/ it.classifier == Any::class
                }

                if (hasAnyResult && (hasListParam || hasAnyParams)) {
                    yield(parseName(member.name) to member)
                }
            }
        }
    }
}

private fun parseName(name: String): String {
    val buf = StringBuilder()

    var index = if (name.startsWith("__")) 2 else 0
    while (index < name.length) {
        val c = name[index]

        if (c == '_') {
            buf.append(
                name.substring(index + 1, index + 3).toInt(16).toChar()
            )
            index += 2
        } else {
            buf.append(c)
        }

        ++index
    }

    return buf.toString()
}
