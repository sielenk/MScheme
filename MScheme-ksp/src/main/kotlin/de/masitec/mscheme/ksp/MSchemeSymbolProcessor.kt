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

@file:OptIn(KspExperimental::class)

package de.masitec.mscheme.ksp

import com.google.devtools.ksp.KspExperimental
import com.google.devtools.ksp.getAnnotationsByType
import com.google.devtools.ksp.processing.Resolver
import com.google.devtools.ksp.processing.SymbolProcessor
import com.google.devtools.ksp.processing.SymbolProcessorEnvironment
import com.google.devtools.ksp.symbol.KSAnnotated
import com.google.devtools.ksp.symbol.KSFunctionDeclaration
import de.masitec.mscheme.ksp.annotations.ScmFunction


class MSchemeSymbolProcessor(
    val environment: SymbolProcessorEnvironment
) : SymbolProcessor {
    override fun process(resolver: Resolver): List<KSAnnotated> {
        environment.logger.info("MSchemeSymbolProcessor.process invoked")

        // Find symbols annotated with @MSchemeBuiltins
        val symbols =
            resolver.getSymbolsWithAnnotation(ScmFunction::class.qualifiedName!!)

        // Process symbols
        symbols.filterIsInstance<KSFunctionDeclaration>()
            .forEach { functionDeclaration ->
                val functionName = functionDeclaration.qualifiedName
                val annotation: ScmFunction = functionDeclaration
                    .getAnnotationsByType(ScmFunction::class)
                    .first()

                if (functionName != null) {
                    environment.logger.logging("'${functionName.asString()}' -> '${annotation.name}'")
                    // generateRuntimeReflectionReplacement(classDeclaration)
                }
            }

        return emptyList()
    }
}
