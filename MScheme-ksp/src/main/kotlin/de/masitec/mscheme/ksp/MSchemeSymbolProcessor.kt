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

package de.masitec.mscheme.ksp

import com.google.devtools.ksp.KspExperimental
import com.google.devtools.ksp.getDeclaredFunctions
import com.google.devtools.ksp.getDeclaredProperties
import com.google.devtools.ksp.getKotlinClassByName
import com.google.devtools.ksp.isPublic
import com.google.devtools.ksp.processing.Dependencies
import com.google.devtools.ksp.processing.Resolver
import com.google.devtools.ksp.processing.SymbolProcessor
import com.google.devtools.ksp.processing.SymbolProcessorEnvironment
import com.google.devtools.ksp.symbol.KSAnnotated
import com.google.devtools.ksp.symbol.KSFile


class MSchemeSymbolProcessor(
    val environment: SymbolProcessorEnvironment
) : SymbolProcessor {
    var iteration = 0

    @OptIn(KspExperimental::class)
    override fun process(resolver: Resolver): List<KSAnnotated> {
        if (iteration++ > 0) {
            return emptyList()
        }

        environment.logger.info("MSchemeSymbolProcessor.process invoked")

        val builtinsDecl = resolver.getKotlinClassByName(SCM_BUILTINS_OBJECT)

        if (builtinsDecl == null) {
            environment.logger.error(
                "MScheme KSP: Cannot find object '$SCM_BUILTINS_OBJECT' on the KSP classpath. " +
                        "Ensure the source is in this source set or available as classpath input."
            )

            return emptyList()
        }

        // Only generate the code when the builtins class is actually compiled.
        // Otherwise, we get a second, identical file in the test source set.
        val builtinsFile = builtinsDecl.containingFile ?: return emptyList()

        val scmFuncDecl = resolver.getKotlinClassByName(SCM_FUNCTION_TYPE)
        if (scmFuncDecl == null) {
            environment.logger.error(
                "MScheme KSP: Cannot resolve required type '$SCM_FUNCTION_TYPE'.",
                builtinsFile
            )
            return emptyList()
        }

        val scmListDecl = resolver.getKotlinClassByName(SCM_LIST_TYPE)
        if (scmListDecl == null) {
            environment.logger.error(
                "MScheme KSP: Cannot resolve required type '$SCM_LIST_TYPE'.",
                builtinsFile
            )
            return emptyList()
        }

        val scmListType = scmListDecl.asStarProjectedType()
        val scmFuncType = scmFuncDecl.asStarProjectedType()
        val anyType = resolver.builtIns.anyType.makeNullable()

        val members = sequence {
            for (propDecl in builtinsDecl.getDeclaredProperties()) {
                if (!propDecl.isPublic())
                    continue

                val propType = propDecl.type.resolve()

                if (scmFuncType.isAssignableFrom(propType)) {
                    val name = propDecl.simpleName.asString()

                    yield(Member.Property(name))
                }
            }

            for (funcDecl in builtinsDecl.getDeclaredFunctions()) {
                if (!funcDecl.isPublic() || funcDecl.qualifiedName == null) {
                    continue
                }

                val name = funcDecl.simpleName.asString()
                val suffix = funcDecl.parameters.joinToString("") { param ->
                    when (param.type.resolve()) {
                        anyType -> "A"
                        scmListType -> "L"
                        else -> "X"
                    }
                }

                yield(Member.Function(suffix, name))
            }
        }

        emitFile(members, builtinsFile)

        return emptyList()
    }

    private fun emitFile(members: Sequence<Member>, sourceFile: KSFile) {
        val file = environment.codeGenerator.createNewFile(
            Dependencies(aggregating = false, sources = arrayOf(sourceFile)),
            FUNCTIONS_PACKAGE_PATH,
            "getBuiltins"
        )

        file.bufferedWriter().use { out ->
            out.appendLine("package $FUNCTIONS_PACKAGE_PATH")
            out.appendLine()
            out.appendLine("import $SCM_FUNCTION_TYPE")
            out.appendLine("import kotlin.reflect.KFunction")
            out.appendLine()
            out.appendLine()
            out.appendLine("fun getBuiltins(): Sequence<Pair<String, Function>> = sequence {")
            for (member in members) {
                out.appendLine("    yield($member)")
            }
            out.appendLine("}")
        }
    }


    companion object {
        const val PACKAGE_NAME = "de.masitec.mscheme"
        const val FUNCTIONS_PACKAGE_PATH = "$PACKAGE_NAME.values.functions"
        const val SCM_BUILTINS_OBJECT = "$FUNCTIONS_PACKAGE_PATH.Builtins"
        const val SCM_FUNCTION_TYPE = "$PACKAGE_NAME.values.Function"
        const val SCM_LIST_TYPE = "$PACKAGE_NAME.values.IList"

        fun parseName(name: String): String {
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
    }

    private sealed class Member(
        val name: String, val check: String, val delimiter: String
    ) {
        class Function(suffix: String, name: String) :
            Member(name, "func$suffix", "::")

        class Property(name: String) :
            Member(name, "prop", ".")

        override fun toString(): String =
            "\"${parseName(name)}\" to $check(Builtins$delimiter$name)"
    }
}
