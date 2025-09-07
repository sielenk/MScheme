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

plugins {
    kotlin("jvm").version("2.2.0")
    id("com.google.devtools.ksp").version("2.2.0-2.0.2")
    id("application")
}

val generatedDir: Provider<Directory> =
    layout.buildDirectory.dir("generated/mscheme/main/kotlin")

repositories {
    mavenCentral()
}

val kotestVersion = "6.0.1"

dependencies {
    ksp(project(":MScheme-ksp"))

    implementation("org.jetbrains.kotlin:kotlin-reflect")

    testImplementation("junit:junit:4.13.1")
    testImplementation("io.kotest:kotest-assertions-core:$kotestVersion")
    testImplementation("io.kotest:kotest-property:$kotestVersion")
}

application {
    mainClass = "mscheme.Main"
}

val createInit = tasks.register("createInit") {
    val inputFiles = fileTree("src/main/scheme") {
        include("**/*.scm")
    }
    val outputFileProvider = generatedDir.map {
        it.file("de/masitec/mscheme/Init.kt")
    }

    inputs.files(inputFiles)
    outputs.files(outputFileProvider)

    doLast {
        val outputFile = outputFileProvider.get().asFile
        val body = inputFiles.map { inputFile ->
            val name = inputFile.name.removeSuffix(".scm").uppercase()
            val valueIn = inputFile.readText()
            val valueOut = valueIn
                .replace(Regex(";.*"), "")
                .replace(Regex("""(["\\])""")) { "\\${ it.groupValues[1]}" } // quote quotes and backslashes
                .replace("\n", " ") // convert to one line
                .replace(Regex("""\s+"""), " ") // squeeze whitespace
                .replace(Regex(""" *([()\[\]]) *""")) { it.groupValues[1] } // remove whitespace around parenthesis

            "    const val $name = \"$valueOut\""
        }

        val fileContent = listOf(
            "package de.masitec.mscheme",
            "",
            "object Init",
            '{'
        ) + body + listOf("}")

        mkdir(outputFile.parent)

        outputFile.writeText(
            fileContent.joinToString(separator = "\n", postfix = "\n")
        )
    }
}


sourceSets {
    main {
        kotlin {
            srcDir(generatedDir)
            srcDir("build/generated/ksp/main/kotlin")
        }
    }
    test {
        kotlin {
            srcDir("build/generated/ksp/test/kotlin")
        }
    }
}

tasks.matching {
    it.name in setOf("kspKotlin", "compileKotlin")
}.configureEach {
    dependsOn(createInit)
}
