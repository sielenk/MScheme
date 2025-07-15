/* An extended Awt TextArea which provides a Reader and Writer.
   Copyright (C) 2001  Marvin H. Sielenkemper

This file is part of MScheme.

MScheme is free software; you can redistribute it and/or modify 
it under the terms of the GNU General Public License as published by 
the Free Software Foundation; either version 2 of the License, 
or (at your option) any later version. 

MScheme is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details. 

You should have received a copy of the GNU General Public License
along with MScheme; see the file COPYING. If not, write to 
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA  02111-1307, USA. */
package mscheme

import java.awt.AWTEventMulticaster
import java.awt.TextArea
import java.awt.event.*
import java.io.IOException
import java.io.InterruptedIOException
import java.io.Reader
import java.io.Writer

internal class StdioReader : Reader(Any()) {
    private var _closed = false
    private var _buffer = ""

    private fun _getChars(len: Int): CharArray {
        synchronized(lock) {
            if (_closed) {
                throw IOException("Stream closed")
            }
            while (_buffer.length < len) {
                try {
                    (lock as Object).wait()
                } catch (e: InterruptedException) {
                    throw InterruptedIOException(e.message)
                }
            }

            val indexOfEot = _buffer.indexOf('\u0004')
            val eotInResult = (indexOfEot != -1) && (indexOfEot < len)
            val resultLen = if (eotInResult) indexOfEot else len

            val result = CharArray(resultLen)
            _buffer.toCharArray(result, 0, 0, resultLen)

            _buffer = _buffer.substring(
                if (eotInResult)
                    indexOfEot + 1 // skip the eot
                else
                    len
            )
            return result
        }
    }

    override fun ready(): Boolean {
        synchronized(lock) {
            return !_closed && _buffer.isNotEmpty()
        }
    }

    override fun read(): Int {
        val result = _getChars(1)

        return if (result.isEmpty())
            -1
        else
            result[0].code
    }

    override fun read(data: CharArray, offs: Int, len: Int): Int {
        val result = _getChars(len)
        val resultLen = result.size

        System.arraycopy(result, 0, data, offs, resultLen)
        return resultLen
    }

    override fun close() {
        synchronized(lock) {
            _closed = true
        }
    }


    fun fillBuffer(s: String) {
        synchronized(lock) {
            if (!_closed) {
                _buffer = _buffer + s
                (lock as Object).notify()
            }
        }
    }
}

internal class StdioWriter(private val _owner: StdioArea) : Writer(Any()) {
    private var _closed = false

    override fun write(data: CharArray, offs: Int, len: Int) {
        write(String(data, offs, len))
    }

    override fun write(data: Int) {
        write("" + data.toChar())
    }

    override fun write(data: String, offs: Int, len: Int) {
        write(data.substring(offs, offs + len))
    }

    override fun write(data: String) {
        synchronized(lock) {
            if (_closed) {
                throw IOException("Stream closed")
            }
            _owner.print(data)
        }
    }

    override fun flush() {
    }

    override fun close() {
        synchronized(lock) {
            _closed = true
        }
    }
}


class StdioArea : TextArea, FocusListener, KeyListener, MouseListener, TextListener {
    /***** interface FocusListener begin  */
    override fun focusGained(e: FocusEvent) {
        _update()
    }

    override fun focusLost(e: FocusEvent) {
    }

    /***** interface FocusListener end  */
    /***** interface MouseListener begin  */
    override fun mouseEntered(e: MouseEvent) {
        _update()
    }

    override fun mouseExited(e: MouseEvent) {
    }

    override fun mousePressed(e: MouseEvent) {
    }

    override fun mouseReleased(e: MouseEvent) {
        _update()
    }

    override fun mouseClicked(e: MouseEvent) {
    }

    /***** interface MouseListener end  */
    /***** interface KeyListener begin  */
    override fun keyPressed(e: KeyEvent) {
        val c = e.getKeyChar()

        if ((c != KeyEvent.CHAR_UNDEFINED)
            &&
            ((getCaretPosition() < _lineBufferStart())
                    ||
                    (c == '\n')
                    )
        ) {
            setCaretPosition(
                _lineBufferEnd()
            )

            _update()
        }
    }

    override fun keyReleased(e: KeyEvent) {
        _update()
    }

    override fun keyTyped(e: KeyEvent) {
        val c = e.getKeyChar()

        if ((c == '\u0004') && _isEmptyLineBuffer()) {
            _stdin.fillBuffer("\u0004")
        }
    }

    /***** interface KeyListener end  */
    /***** interface TextListener begin  */
    private var _oldSelStart = 0
    private var _oldSelEnd = 0

    private fun _updateSelection() {
        _oldSelStart = getSelectionStart()
        _oldSelEnd = getSelectionEnd()
    }

    override fun textValueChanged(e: TextEvent) {
        val newSelEnd = getSelectionEnd()

        if (newSelEnd < _oldSelStart) {
            // The del-key deletes backward from _oldSelStart.
            // Preted the deleted char was selected.
            _oldSelStart = newSelEnd
        }

        if (_lineBufferStart() > _oldSelStart) {
            // the change isn't completely inside the line buffer

            if (_lineBufferStart() >= _oldSelEnd) {
                // the change is completely outside the line buffer

                _lineBufferStart(
                    _lineBufferStart()
                            - _oldSelEnd
                            + newSelEnd
                )
            } else {
                // the line buffer starts inside the changed text

                _lineBufferStart(
                    _oldSelStart
                )
            }
        }

        _oldSelStart = getSelectionStart()
        _oldSelEnd = newSelEnd

        _rescanLineBuffer()
    }

    /***** interface TextListener end  */
    /***** line buffer handling begin  */
    private var _lineBufferStartVar = -1

    private fun _disableLineBuffer() {
        _lineBufferStartVar = -1
    }

    private fun _enableLineBuffer() {
        _lineBufferStart(_lineBufferEnd())
    }

    private fun _lineBufferStart(): Int =
        if (_lineBufferStartVar != -1)
            _lineBufferStartVar
        else
            _lineBufferEnd()

    private fun _lineBufferStart(newValue: Int): Int {
        val oldValue = _lineBufferStartVar
        _lineBufferStartVar = newValue
        return oldValue
    }

    private fun _lineBufferEnd(): Int =
        getText().length

    private fun _clearLineBuffer() {
        replaceRange(
            "",
            _lineBufferStart(),
            _lineBufferEnd()
        )
    }

    private fun _lineBuffer(): String =
        getText().substring(
            _lineBufferStart(),
            _lineBufferEnd()
        )

    private fun _isEmptyLineBuffer(): Boolean =
        _lineBufferStart() == _lineBufferEnd()

    private fun _rescanLineBuffer() {
        while (!_isEmptyLineBuffer()) {
            var line: String? = null

            synchronized(this) {
                val i = getText().indexOf(
                    '\n',
                    _lineBufferStart()
                )
                if (i != -1) {
                    val newStart = i + 1
                    line = getText().substring(
                        _lineBufferStart(newStart),
                        newStart
                    )
                }
            }

            if (line == null) {
                return
            }

            _stdin.fillBuffer(line)
        }
    }

    private fun _selectionInLineBuffer(): Boolean =
        getSelectionStart() >= _lineBufferStart()

    private fun _caretInLineBuffer(): Boolean =
        getCaretPosition() >= _lineBufferStart()

    /***** line buffer handling end  */
    /***** action event handling begin  */
    var _actionListener: ActionListener? = null

    private var _oldState = 0

    private fun _update() {
        _updateSelection()

        val newState =
            ((if (canCut()) 1 else 0)
                    + (if (canCopy()) 2 else 0)
                    + (if (canPaste()) 4 else 0))

        if (_oldState != newState) {
            _oldState = newState
            _dispatchAction()
        }
    }

    private fun _dispatchAction(l: ActionListener? = _actionListener) {
        l?.actionPerformed(
            ActionEvent(
                this,
                ActionEvent.ACTION_PERFORMED,
                this.actionCommand
            )
        )
    }

    val actionCommand: String
        get() = "updateCopyCutPaste"

    fun addActionListener(l: ActionListener) {
        _actionListener = AWTEventMulticaster.add(_actionListener, l)
        _dispatchAction(l)
    }

    fun removeActionListener(l: ActionListener) {
        _actionListener = AWTEventMulticaster.remove(_actionListener, l)
    }

    /***** action event handling end  */
    /***** copy and paste support begin  */
    private var _clipboard = ""

    @Synchronized
    private fun _copy(): String =
        selectedText

    @Synchronized
    private fun _paste(s: String?) {
        insert(
            s,
            getCaretPosition()
        )
    }

    @Synchronized
    fun clear() {
        _disableLineBuffer()
        setText("")
        _enableLineBuffer()
        _update()
    }

    fun copy() {
        _clipboard = _copy()
        _update()
    }

    fun cut() {
        copy()
        replaceRange(
            "",
            getSelectionStart(),
            getSelectionEnd()
        )
        _update()
    }

    fun paste() {
        _paste(_clipboard)
        _update()
    }

    @Synchronized
    fun canCopy(): Boolean =
        getSelectionStart() != getSelectionEnd()

    @Synchronized
    fun canCut(): Boolean =
        canCopy() && _selectionInLineBuffer()

    fun canPaste(): Boolean =
        _clipboard.isNotEmpty()
            && _caretInLineBuffer()

    /***** copy and paste support end  */
    /***** text io begin  */
    private val _stdin = StdioReader()
    private val _stdout = StdioWriter(this)

    @Synchronized
    fun print(text: String?) {
        val delta = getCaretPosition() - _lineBufferStart()
        val lineBufferContent = _lineBuffer()

        _clearLineBuffer()
        _disableLineBuffer()
        append(text)
        _enableLineBuffer()
        append(lineBufferContent)
        if (delta >= 0) {
            setCaretPosition(_lineBufferStart() + delta)
        }
        _update()
    }

    fun stdin(): Reader =
        _stdin

    fun stdout(): Writer =
        _stdout

    /***** text io end  */
    /***** constructors begin  */
    private fun _init() {
        addKeyListener(this)
        addMouseListener(this)
        addTextListener(this)
        _enableLineBuffer()
        _update()
    }

    constructor(text: String?, rows: Int, columns: Int, scrollbars: Int) : super(
        text,
        rows,
        columns,
        scrollbars
    ) {
        _init()
    }

    constructor(text: String?, rows: Int, columns: Int) : super(text, rows, columns) {
        _init()
    }

    constructor(rows: Int, columns: Int) : super(rows, columns) {
        _init()
    }

    constructor(text: String?) : super(text) {
        _init()
    }

    constructor() : super() {
        _init()
    }

    /***** constructors end  */

    companion object {
        private const val serialVersionUID = 1L
    }
}
