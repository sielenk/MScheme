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

package mscheme;

import java.awt.AWTEventMulticaster;
import java.awt.TextArea;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.TextEvent;
import java.awt.event.TextListener;
import java.io.IOException;
import java.io.InterruptedIOException;
import java.io.Reader;
import java.io.Writer;


class StdioReader extends Reader {

  public final static String CVS_ID
      = "$Id$";


  private boolean _closed;
  private String _buffer;

  public StdioReader() {
    super(new Object());

    _buffer = "";
    _closed = false;
  }

  private char[] _getChars(int len)
      throws IOException {
    synchronized (lock) {
      if (_closed) {
        throw new IOException("Stream closed");
      }

      while (_buffer.length() < len) {
        try {
          lock.wait();
        } catch (InterruptedException e) {
          throw new InterruptedIOException(e.getMessage());
        }
      }

      int indexOfEot = _buffer.indexOf('\004');
      boolean eotInResult = (indexOfEot != -1) && (indexOfEot < len);
      int resultLen = eotInResult ? indexOfEot : len;

      char[] result = new char[resultLen];
      _buffer.getChars(0, resultLen, result, 0);

      _buffer = _buffer.substring(
          eotInResult
              ? indexOfEot + 1  // skip the eot
              : len
      );

      return result;
    }
  }

  public boolean ready() {
    synchronized (lock) {
      return !_closed && (_buffer.length() > 0);
    }
  }

  public int read()
      throws IOException {
    char[] result = _getChars(1);

    return
        (result.length == 0)
            ? -1
            : result[0];
  }

  public int read(char[] data, int offs, int len)
      throws IOException {
    char[] result = _getChars(len);
    int resultLen = result.length;

    System.arraycopy(result, 0, data, offs, resultLen);
    return resultLen;
  }

  public void close() {
    synchronized (lock) {
      _closed = true;
    }
  }


  public void fillBuffer(String s) {
    synchronized (lock) {
      if (!_closed) {
        _buffer = _buffer + s;
        lock.notify();
      }
    }
  }
}

class StdioWriter extends Writer {

  public final static String CVS_ID
      = "$Id$";


  private boolean _closed;
  private final StdioArea _owner;

  public StdioWriter(StdioArea owner) {
    super(new Object());

    _closed = false;
    _owner = owner;
  }

  public void write(char[] data, int offs, int len)
      throws IOException {
    write(new String(data, offs, len));
  }

  public void write(int data)
      throws IOException {
    write("" + (char) data);
  }

  public void write(String data, int offs, int len)
      throws IOException {
    write(data.substring(offs, offs + len));
  }

  public void write(String data)
      throws IOException {
    synchronized (lock) {
      if (_closed) {
        throw new IOException("Stream closed");
      }

      _owner.print(data);
    }
  }

  public void flush() {
  }

  public void close() {
    synchronized (lock) {
      _closed = true;
    }
  }
}


public class StdioArea
    extends TextArea
    implements FocusListener, KeyListener, MouseListener, TextListener {

  /**
   *
   */
  private static final long serialVersionUID = 1L;
  public final static String CVS_ID
      = "$Id$";


  /***** interface FocusListener begin *****/

  public void focusGained(FocusEvent e) {
    _update();
  }

  public void focusLost(FocusEvent e) {
  }

  /***** interface FocusListener end *****/

  /***** interface MouseListener begin *****/

  public void mouseEntered(MouseEvent e) {
    _update();
  }

  public void mouseExited(MouseEvent e) {
  }

  public void mousePressed(MouseEvent e) {
  }

  public void mouseReleased(MouseEvent e) {
    _update();
  }

  public void mouseClicked(MouseEvent e) {
  }

  /***** interface MouseListener end *****/

  /***** interface KeyListener begin *****/

  public void keyPressed(KeyEvent e) {
    char c = e.getKeyChar();

    if (
        (c != KeyEvent.CHAR_UNDEFINED)
            &&
            (
                (getCaretPosition() < _lineBufferStart())
                    ||
                    (c == '\n')
            )
    ) {
      setCaretPosition(
          _lineBufferEnd()
      );

      _update();
    }
  }

  public void keyReleased(KeyEvent e) {
    _update();
  }

  public void keyTyped(KeyEvent e) {
    char c = e.getKeyChar();

    if ((c == '\004') && _isEmptyLineBuffer()) {
      _stdin.fillBuffer("\004");
    }
  }

  /***** interface KeyListener end *****/

  /***** interface TextListener begin *****/

  private int _oldSelStart = 0;
  private int _oldSelEnd = 0;

  private void _updateSelection() {
    _oldSelStart = getSelectionStart();
    _oldSelEnd = getSelectionEnd();
  }

  public void textValueChanged(TextEvent e) {
    int newSelEnd = getSelectionEnd();

    if (newSelEnd < _oldSelStart) {
      // The del-key deletes backward from _oldSelStart.
      // Preted the deleted char was selected.
      _oldSelStart = newSelEnd;
    }

    if (_lineBufferStart() > _oldSelStart) {
      // the change isn't completely inside the line buffer

      if (_lineBufferStart() >= _oldSelEnd) {
        // the change is completely outside the line buffer

        _lineBufferStart(
            _lineBufferStart()
                - _oldSelEnd
                + newSelEnd
        );
      } else {
        // the line buffer starts inside the changed text

        _lineBufferStart(
            _oldSelStart
        );
      }
    }

    _oldSelStart = getSelectionStart();
    _oldSelEnd = newSelEnd;

    _rescanLineBuffer();
  }

  /***** interface TextListener end *****/

  /***** line buffer handling begin *****/

  private int _lineBufferStartVar = -1;

  private void _disableLineBuffer() {
    _lineBufferStartVar = -1;
  }

  private void _enableLineBuffer() {
    _lineBufferStart(_lineBufferEnd());
  }

  private int _lineBufferStart() {
    return
        (_lineBufferStartVar != -1)
            ? _lineBufferStartVar
            : _lineBufferEnd();
  }

  private int _lineBufferStart(int newValue) {
    int oldValue = _lineBufferStartVar;
    _lineBufferStartVar = newValue;
    return oldValue;
  }

  private int _lineBufferEnd() {
    return getText().length();
  }

  private void _clearLineBuffer() {
    replaceRange(
        "",
        _lineBufferStart(),
        _lineBufferEnd()
    );
  }

  private String _lineBuffer() {
    return getText().substring(
        _lineBufferStart(),
        _lineBufferEnd()
    );
  }

  private boolean _isEmptyLineBuffer() {
    return _lineBufferStart() == _lineBufferEnd();
  }

  private void _rescanLineBuffer() {
    while (!_isEmptyLineBuffer()) {
      String line = null;

      synchronized (this) {
        int i = getText().indexOf(
            '\n',
            _lineBufferStart()
        );

        if (i != -1) {
          int newStart = i + 1;
          line = getText().substring(
              _lineBufferStart(newStart),
              newStart
          );
        }
      }

      if (line == null) {
        return;
      }

      _stdin.fillBuffer(line);
    }
  }

  private boolean _selectionInLineBuffer() {
    return getSelectionStart() >= _lineBufferStart();
  }

  private boolean _caretInLineBuffer() {
    return getCaretPosition() >= _lineBufferStart();
  }

  /***** line buffer handling end *****/

  /***** action event handling begin *****/

  ActionListener _actionListener = null;

  private int _oldState = 0;

  private void _update() {
    _updateSelection();

    int newState =
        (canCut() ? 1 : 0)
            + (canCopy() ? 2 : 0)
            + (canPaste() ? 4 : 0);

    if (_oldState != newState) {
      _oldState = newState;
      _dispatchAction();
    }
  }

  private void _dispatchAction(ActionListener l) {
    if (l != null) {
      l.actionPerformed(
          new ActionEvent(
              this,
              ActionEvent.ACTION_PERFORMED,
              getActionCommand()
          )
      );
    }
  }

  private void _dispatchAction() {
    _dispatchAction(_actionListener);
  }

  public String getActionCommand() {
    return "updateCopyCutPaste";
  }

  public void addActionListener(ActionListener l) {
    _actionListener = AWTEventMulticaster.add(_actionListener, l);
    _dispatchAction(l);
  }

  public void removeActionListener(ActionListener l) {
    _actionListener = AWTEventMulticaster.remove(_actionListener, l);
  }

  /***** action event handling end *****/

  /***** copy and paste support begin *****/

  private String _clipboard = "";

  private synchronized String _copy() {
    return getSelectedText();
  }

  private synchronized void _paste(String s) {
    insert(
        s,
        getCaretPosition()
    );
  }

  public synchronized void clear() {
    _disableLineBuffer();
    setText("");
    _enableLineBuffer();
    _update();
  }

  public void copy() {
    _clipboard = _copy();
    _update();
  }

  public void cut() {
    copy();
    replaceRange(
        "",
        getSelectionStart(),
        getSelectionEnd()
    );
    _update();
  }

  public void paste() {
    _paste(_clipboard);
    _update();
  }

  public synchronized boolean canCopy() {
    return getSelectionStart() != getSelectionEnd();
  }

  public synchronized boolean canCut() {
    return canCopy() && _selectionInLineBuffer();
  }

  public boolean canPaste() {
    return
        (_clipboard.length() > 0)
            && _caretInLineBuffer();
  }

  /***** copy and paste support end *****/

  /***** text io begin *****/

  private final StdioReader _stdin = new StdioReader();
  private final StdioWriter _stdout = new StdioWriter(this);

  public synchronized void print(String text) {
    int delta = getCaretPosition() - _lineBufferStart();
    String lineBufferContent = _lineBuffer();
    _clearLineBuffer();
    _disableLineBuffer();
    append(text);
    _enableLineBuffer();
    append(lineBufferContent);
    if (delta >= 0) {
      setCaretPosition(_lineBufferStart() + delta);
    }
    _update();
  }

  public Reader stdin() {
    return _stdin;
  }

  public Writer stdout() {
    return _stdout;
  }

  /***** text io end *****/

  /***** constructors begin *****/

  private void _init() {
    addKeyListener(this);
    addMouseListener(this);
    addTextListener(this);
    _enableLineBuffer();
    _update();
  }

  public StdioArea(String text, int rows, int columns, int scrollbars) {
    super(text, rows, columns, scrollbars);
    _init();
  }

  public StdioArea(String text, int rows, int columns) {
    super(text, rows, columns);
    _init();
  }

  public StdioArea(int rows, int columns) {
    super(rows, columns);
    _init();
  }

  public StdioArea(String text) {
    super(text);
    _init();
  }

  public StdioArea() {
    super();
    _init();
  }

  /***** constructors end *****/
}
