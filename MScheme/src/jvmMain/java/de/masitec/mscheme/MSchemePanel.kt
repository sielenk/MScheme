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
/* Created on 28.12.2004, 10:03:07 by sielenk */
package de.masitec.mscheme

import de.masitec.mscheme.exceptions.SchemeException
import de.masitec.mscheme.machine.Machine
import de.masitec.mscheme.util.JvmWriter
import de.masitec.mscheme.util.Writer
import java.awt.BorderLayout
import java.awt.Button
import java.awt.Panel
import java.awt.SystemColor
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.io.IOException

/**
 * @author sielenk
 *
 * TODO To change the template for this generated type comment go to Window -
 * Preferences - Java - Code Style - Code Templates
 */
class MSchemePanel : Panel() {
    private var _stdio: StdioArea? = null

    private var _startStopButton: Button? = null

    private var _buttonPanel: Panel? = null

    private var _copyButton: Button? = null

    private var _cutButton: Button? = null

    private var _clearButton: Button? = null

    private var _pasteButton: Button? = null

    private fun initialize() {
        this.setLayout(BorderLayout())
        this.setSize(600, 400)
        this.add(get_buttonPanel(), BorderLayout.SOUTH)
        this.add(get_stdio(), BorderLayout.CENTER)
        get_stdio()!!.requestFocus()
    }

    private fun get_buttonPanel(): Panel {
        if (_buttonPanel == null) {
            _buttonPanel = Panel()
            _buttonPanel!!.setBackground(SystemColor.control)
            _buttonPanel!!.add(get_startStopButton(), null)
            _buttonPanel!!.add(get_clearButton(), null)
            _buttonPanel!!.add(get_copyButton(), null)
            _buttonPanel!!.add(get_cutButton(), null)
            _buttonPanel!!.add(get_pasteButton(), null)
        }
        return _buttonPanel!!
    }

    private fun get_copyButton(): Button? {
        if (_copyButton == null) {
            _copyButton = Button()
            _copyButton!!.setLabel("Copy")
            _copyButton!!.addActionListener(ActionListener { e: ActionEvent? ->
                get_stdio()!!.copy()
                get_stdio()!!.requestFocus()
            })
        }
        return _copyButton
    }

    private fun get_cutButton(): Button? {
        if (_cutButton == null) {
            _cutButton = Button()
            _cutButton!!.setLabel("Cut")
            _cutButton!!.addActionListener(ActionListener { e: ActionEvent? ->
                get_stdio()!!.cut()
                get_stdio()!!.requestFocus()
            })
        }
        return _cutButton
    }

    private fun get_clearButton(): Button? {
        if (_clearButton == null) {
            _clearButton = Button()
            _clearButton!!.setLabel("Clear")
            _clearButton!!.addActionListener(ActionListener { e: ActionEvent? ->
                get_stdio()!!.clear()
                get_stdio()!!.requestFocus()
            })
        }
        return _clearButton
    }

    private fun get_pasteButton(): Button? {
        if (_pasteButton == null) {
            _pasteButton = Button()
            _pasteButton!!.setLabel("Paste")
            _pasteButton!!.addActionListener(ActionListener { e: ActionEvent? ->
                get_stdio()!!.paste()
                get_stdio()!!.requestFocus()
            })
        }
        return _pasteButton
    }

    private fun get_stdio(): StdioArea? {
        if (_stdio == null) {
            _stdio = StdioArea()
            _stdio!!.addActionListener(ActionListener { e: ActionEvent? ->
                get_pasteButton()!!.setEnabled(get_stdio()!!.canPaste())
                get_copyButton()!!.setEnabled(get_stdio()!!.canCopy())
                get_cutButton()!!.setEnabled(get_stdio()!!.canCut())
                get_stdio()!!.requestFocus()
            })
        }
        return _stdio
    }

    private var _runner: Thread? = null

    init {
        initialize()
    }

    @Synchronized
    fun start() {
        if (_runner != null) {
            return
        }

        this._runner = Thread(Runnable {
            try {
                get_startStopButton()!!.setLabel("Stop")
                Machine(
                    get_stdio()!!.stdin(),
                    JvmWriter(get_stdio()!!.stdout())
                ).unprotectedRun()
            } catch (e: SchemeException) {
                try {
                    get_stdio()!!.stdout().write(e.message)
                } catch (e1: IOException) {
                }
            } catch (e: InterruptedException) {
            } finally {
                _runner = null
                get_startStopButton()!!.setLabel("Start")
                get_startStopButton()!!.setEnabled(true)
            }
        })
        _runner!!.start()
    }

    fun stop() {
        val _localRunner = _runner

        if (_localRunner == null || _localRunner.isInterrupted()) {
            return
        }

        get_startStopButton()!!.setEnabled(false)
        _localRunner.interrupt()
    }

    private fun runnerStartStop() {
        if (_runner == null) {
            start()
        } else {
            stop()
        }
    }

    private fun get_startStopButton(): Button? {
        if (_startStopButton == null) {
            _startStopButton = Button()
            _startStopButton!!.setLabel("Start")
            _startStopButton!!
                .addActionListener { e: ActionEvent? ->
                    runnerStartStop()
                    get_stdio()!!.requestFocus()
                }
        }
        return _startStopButton
    }

    companion object {
        private const val serialVersionUID = 1L
    }
}
