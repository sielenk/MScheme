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

val generatedDir = layout.buildDirectory.map {
    it.dir("generated-src")
}

repositories {
    mavenCentral()
}

dependencies {
    implementation("org.jetbrains.kotlin:kotlin-reflect")
    testImplementation("junit:junit:4.13.1")

    implementation(project(":MScheme-ksp"))
    ksp(project(":MScheme-ksp"))
}

application {
    mainClass = "mscheme.Main"
}

val createInit = tasks.register("createInit") {
    val inputFiles = fileTree("src/main/scheme") {
        include("**/*.scm")
    }
    val outputFileProvider = generatedDir.map {
        it.dir("mscheme").file("IInit.java")
    }

    inputs.files(inputFiles)
    outputs.files(outputFileProvider)

    doLast {
        val outputFile = outputFileProvider.get().asFile
        val body = inputFiles.map {
            val name = it.name.removeSuffix(".scm").uppercase()
            val valueIn = it.readText()
            val valueOut = valueIn
                .replace(Regex(";.*"), "")
                .replace(Regex("""(["\\])""")) { "\\${it.groupValues[1]}" } // quote quotes and backslashes
                .replace("\n", " ") // convert to one line
                .replace(Regex("""\s+"""), " ") // squeeze whitespace
                .replace(Regex(""" *([()\[\]]) *""")) { it.groupValues[1] } // remove whitespace around parenthesis

            "    String $name = \"$valueOut\";"
        }

        val fileContent = listOf(
            "package mscheme;",
            "",
            "public interface IInit",
            '{'
        ) + body + listOf("}")

        mkdir(outputFile.parent)

        outputFile.writeText(fileContent.joinToString("\n"))
    }
}


sourceSets {
    main {
        java {
            srcDir(generatedDir)
        }
        kotlin {
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
