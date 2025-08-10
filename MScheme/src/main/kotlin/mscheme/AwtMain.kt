/*
 * Awt main class for MScheme. Copyright (C) 2001 Marvin H. Sielenkemper
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
package mscheme

import java.awt.BorderLayout
import java.awt.Frame
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent
import kotlin.system.exitProcess


class AwtMain {
    private val frame: Frame by lazy {
        val newFrame = Frame()

        newFrame.setTitle("MScheme")
        newFrame.setSize(600, 400)
        newFrame.add(panel, BorderLayout.CENTER)
        newFrame.addWindowListener(object : WindowAdapter() {
            override fun windowClosing(e: WindowEvent) {
                panel.stop()
                newFrame.isVisible = false
                newFrame.dispose()
            }

            override fun windowClosed(e: WindowEvent?) {
                exitProcess(0)
            }
        })

        newFrame
    }

    private val panel: MSchemePanel by lazy {
        MSchemePanel()
    }

    companion object {
        @JvmStatic
        fun main(argv: Array<String>) {
            AwtMain().frame.isVisible = true
        }
    }
}