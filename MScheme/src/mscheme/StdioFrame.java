/* TODO Add short description of this file here.
 Copyright (C) 2004 Marvin H. Sielenkemper

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

/* Created on 28.12.2004, 10:03:07 by sielenk */

package mscheme;

import java.awt.Frame;

import java.awt.Panel;
import java.awt.Button;

import mscheme.machine.Machine;

/**
 * @author sielenk
 * 
 * TODO To change the template for this generated type comment go to Window -
 * Preferences - Java - Code Style - Code Templates
 */
public class StdioFrame
        extends Frame
        implements Runnable
{
    public final static String CVS_ID = "$Id$";

    private StdioArea _stdio = null;

    private Panel _buttonPanel = null;

    private Button _copyButton = null;

    private Button _cutButton = null;

    private Button _clearButton = null;

    private Button _pasteButton = null;

    public StdioFrame()
    {
        super();
        initialize();
    }

    private void initialize()
    {
        this.setSize(600, 400);
        this.setTitle("MScheme Frame");
        this.add(get_buttonPanel(), java.awt.BorderLayout.SOUTH);
        this.add(get_stdio(), java.awt.BorderLayout.CENTER);
        this.addWindowListener(new java.awt.event.WindowAdapter()
        {
            public void windowOpened(java.awt.event.WindowEvent e)
            {
                _runner.start();
                get_stdio().requestFocus();
            }

            public void windowClosing(java.awt.event.WindowEvent e)
            {
                _runner.interrupt();
                dispose();
            }
        });
    }

    private Panel get_buttonPanel()
    {
        if (_buttonPanel == null)
        {
            _buttonPanel = new Panel();
            _buttonPanel.add(get_clearButton(), null);
            _buttonPanel.add(get_copyButton(), null);
            _buttonPanel.add(get_cutButton(), null);
            _buttonPanel.add(get_pasteButton(), null);
        }
        return _buttonPanel;
    }

    private Button get_copyButton()
    {
        if (_copyButton == null)
        {
            _copyButton = new Button();
            _copyButton.setLabel("Copy");
            _copyButton.addActionListener(new java.awt.event.ActionListener()
            {
                public void actionPerformed(java.awt.event.ActionEvent e)
                {
                    get_stdio().copy();
                    get_stdio().requestFocus();
                }
            });
        }
        return _copyButton;
    }

    private Button get_cutButton()
    {
        if (_cutButton == null)
        {
            _cutButton = new Button();
            _cutButton.setLabel("Cut");
            _cutButton.addActionListener(new java.awt.event.ActionListener()
            {
                public void actionPerformed(java.awt.event.ActionEvent e)
                {
                    get_stdio().cut();
                    get_stdio().requestFocus();
                }
            });
        }
        return _cutButton;
    }

    private Button get_clearButton()
    {
        if (_clearButton == null)
        {
            _clearButton = new Button();
            _clearButton.setLabel("Clear");
            _clearButton.addActionListener(new java.awt.event.ActionListener()
            {
                public void actionPerformed(java.awt.event.ActionEvent e)
                {
                    get_stdio().clear();
                    get_stdio().requestFocus();
                }
            });
        }
        return _clearButton;
    }

    private Button get_pasteButton()
    {
        if (_pasteButton == null)
        {
            _pasteButton = new Button();
            _pasteButton.setLabel("Paste");
            _pasteButton.addActionListener(new java.awt.event.ActionListener()
            {
                public void actionPerformed(java.awt.event.ActionEvent e)
                {
                    get_stdio().paste();
                    get_stdio().requestFocus();
                }
            });
        }
        return _pasteButton;
    }

    private StdioArea get_stdio()
    {
        if (_stdio == null)
        {
            _stdio = new StdioArea();
            _stdio.addActionListener(new java.awt.event.ActionListener()
            {
                public void actionPerformed(java.awt.event.ActionEvent e)
                {
                    get_pasteButton().setEnabled(get_stdio().canPaste());
                    get_copyButton().setEnabled(get_stdio().canCopy());
                    get_cutButton().setEnabled(get_stdio().canCut());
                    get_stdio().requestFocus();
                }
            });
        }
        return _stdio;
    }

    /** *** interface Runnable begin **** */

    private final Thread _runner = new Thread(this);

    public void run()
    {
        new Machine(get_stdio().stdin(), get_stdio().stdout()).run();
        dispose();
    }

    /** *** interface Runnable end **** */

    public void start()
    {
        if (!_runner.isAlive())
        {
            _runner.start();
        }
    }
}