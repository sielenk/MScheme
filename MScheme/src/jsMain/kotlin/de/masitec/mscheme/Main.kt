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

package de.masitec.mscheme

import de.masitec.mscheme.machine.Machine
import de.masitec.mscheme.util.Writer
import de.masitec.mscheme.util.createStdInReader
import de.masitec.mscheme.util.createStringReader
import de.masitec.mscheme.values.InputPort
import de.masitec.mscheme.values.OutputPort
import de.masitec.mscheme.values.ValueTraits
import kotlinx.browser.document
import org.w3c.dom.HTMLButtonElement
import org.w3c.dom.HTMLElement
import org.w3c.dom.HTMLTextAreaElement
import org.w3c.dom.events.KeyboardEvent

fun main() {
    val outputEl = document.getElementById("scheme-output") as HTMLElement
    val inputEl = document.getElementById("scheme-input") as HTMLTextAreaElement
    val evalBtn = document.getElementById("scheme-eval") as HTMLButtonElement

    val outputWriter = object : Writer {
        override fun write(i: Int) { outputEl.textContent += i.toChar().toString() }
        override fun write(s: String) { outputEl.textContent += s }
        override fun flush() {}
        override fun close() {}
    }

    val machine = Machine(createStdInReader(), outputWriter)
    val outputPort = OutputPort.create(outputWriter)

    fun evaluate() {
        val input = inputEl.value.trim()
        if (input.isEmpty()) return
        inputEl.value = ""

        outputEl.textContent += "> $input\n"

        try {
            createStringReader(input).use { reader ->
                val port = InputPort.create(reader)
                while (true) {
                    val expr = port.read()
                    if (ValueTraits.eq(expr, InputPort.EOF_VALUE)) break
                    val result = machine.evaluate(expr)
                    if (result != null) {
                        outputPort.write(result)
                        outputEl.textContent += "\n"
                    }
                }
            }
        } catch (e: Throwable) {
            outputEl.textContent += "${e.message}\n"
        }

        outputEl.scrollTop = outputEl.scrollHeight.toDouble()
    }

    evalBtn.onclick = { evaluate() }

    inputEl.addEventListener("keydown", { event ->
        val keyEvent = event as KeyboardEvent
        if (keyEvent.key == "Enter" && !keyEvent.shiftKey) {
            event.preventDefault()
            evaluate()
        }
    })
}
