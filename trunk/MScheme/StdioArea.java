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

package MScheme;

import java.awt.TextArea;

import java.awt.AWTEventMulticaster;

import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import java.io.Reader;
import java.io.Writer;
import java.io.IOException;

import java.util.Vector;


class StdioReader extends Reader
{
    public final static String id
        = "$Id$";


    private boolean _closed;
    private String  _buffer;

    public StdioReader()
    {
        super(new Object());

        _buffer = "";
        _closed = false;
    }

    private char[] _getChars(int len)
        throws IOException
    {
        synchronized(lock)
        {
            if (_closed)
            {
                throw new IOException("Stream closed");
            }

            while (_buffer.length() < len)
            {
                try
                {
                    lock.wait();
                }
                catch (Exception exception) { }
            }

            char result[] = new char[len];
            _buffer.getChars(0, len, result, 0);
            _buffer = _buffer.substring(len);
            return result;
        }
    }

    public boolean ready()
    {
        synchronized(lock)
        {
            return !_closed && (_buffer.length() > 0);
        }
    }

    public int read()
        throws IOException
    {
        return _getChars(1)[0];
    }

    public int read(char data[], int offs, int len)
        throws IOException
    {
        System.arraycopy(_getChars(len), 0, data, offs, len);
        return len;
    }

    public void close()
    {
        _closed = true;
    }


    public void fillBuffer(String s)
    {
        synchronized(lock)
        {
            _buffer += s;
            lock.notify();
        }
    }
}

class StdioWriter extends Writer
{
    public final static String id
        = "$Id$";


    private boolean   _closed;
    private StdioArea _owner;

    public StdioWriter(StdioArea owner)
    {
        super(new Object());

        _closed      = false;
        _owner       = owner;
    }

    public void write(char data[], int offs, int len)
        throws IOException
    {
        write(new String(data, offs, len));
    }

    public void write(int data)
        throws IOException
    {
        write("" + (char)data);
    }

    public void write(String data, int offs, int len)
        throws IOException
    {
        write(data.substring(offs, offs + len));
    }

    public void write(String data)
        throws IOException
    {
        synchronized(lock)
        {
            if (_closed)
            {
                throw new IOException("Stream closed");
            }

            _owner.print(data);
        }
    }

    public void flush() 
    { }

    public void close() 
    {
        _closed = true;
    }
}


public class StdioArea
    extends    TextArea
    implements KeyListener, MouseListener
{
    public final static String id
        = "$Id$";


    /***** interface MouseListener begin *****/

    public void mouseEntered(MouseEvent e) { }
    public void mouseExited (MouseEvent e) { }

    public void mousePressed (MouseEvent e) { }    
    public void mouseReleased(MouseEvent e) { _update(); }

    public void mouseClicked (MouseEvent e) { }

    /***** interface MouseListener end *****/

    /***** interface KeyListener begin *****/

    public void keyPressed (KeyEvent e) { }
    public void keyReleased(KeyEvent e) { _update(); }

    public void keyTyped(KeyEvent e)
    {
        if (e.getKeyChar() != '\n')
            return;

        String  textString = getText();
        int     lineEnd    = getCaretPosition();
        int     lineBegin  = textString.lastIndexOf('\n', lineEnd - 1) + 1;

        _stdin.fillBuffer(textString.substring(lineBegin, lineEnd) + '\n');
    }

    /***** interface KeyListener end *****/
    
    /***** action event handling begin *****/

    ActionListener _actionListener = null;

    private boolean _oldCanCopyCut = true;
    private boolean _oldCanPaste   = true;

    private void _update()
    {
        boolean newCanCopyCut = canCopyCut();
        boolean newCanPaste   = canPaste  ();

        if ((_oldCanCopyCut ^ newCanCopyCut) || (_oldCanPaste ^ newCanPaste))
        {
            _oldCanCopyCut = newCanCopyCut;
            _oldCanPaste   = newCanPaste;

            _dispatchAction();
        }
    }

    private void _dispatchAction() {
        if (_actionListener != null) {
            _actionListener.actionPerformed(
                new ActionEvent(
                    this,
                    ActionEvent.ACTION_PERFORMED,
                    getActionCommand()
                )
            );
        }
    }

    public String getActionCommand()
    {
        return "updateCopyCutPaste";
    }

    public void addActionListener(ActionListener l) {
        _actionListener = AWTEventMulticaster.add(_actionListener, l);
    }
    public void removeActionListener(ActionListener l) {
        _actionListener = AWTEventMulticaster.remove(_actionListener, l);
    }

    /***** action event handling end *****/

    /***** copy and paste support begin *****/

    private String _clipboard = "";

    private synchronized String _copy()
    {
        return getSelectedText();
    }
    
    private synchronized void _paste(String s)
    {
        replaceRange(
            s,
            getSelectionStart(),
            getSelectionEnd()
        );
    }

    public synchronized void clear()
    {
        setText("");
        _update();
    }

    public void copy()
    {
        _clipboard = _copy();
        _update();
    }

    public void cut()
    {
        copy();
        _paste("");
        _update();
    }

    public void paste()
    {
        _paste(_clipboard);
        _update();
    }

    public synchronized boolean canCopyCut()
    {
        return getSelectionStart() != getSelectionEnd();
    }

    public boolean canPaste()
    {
        return _clipboard.length() > 0;
    }

    /***** copy and paste support end *****/

    /***** text io begin *****/

    private StdioReader _stdin  = new StdioReader();
    private StdioWriter _stdout = new StdioWriter(this);

    public synchronized void print(String text)
    {
        insert(text, getCaretPosition());
    }

    public Reader stdin () { return _stdin;  }
    public Writer stdout() { return _stdout; }

    /***** text io end *****/

    /***** constructors begin *****/
     
    private void _init()
    {
        addKeyListener  (this);
        addMouseListener(this);
    }

    public StdioArea(String text, int rows, int columns, int scrollbars)
    {
        super(text, rows, columns, scrollbars);
        _init();
    }

    public StdioArea(String text, int rows, int columns)
    {
        super(text, rows, columns);
        _init();
    }

    public StdioArea(int rows, int columns)
    {
        super(rows, columns);
        _init();
    }

    public StdioArea(String text)
    {
        super(text);
        _init();
    }

    public StdioArea()
    {
        super();
        _init();
    }

    /***** constructors end *****/
}
