/* Awt panel containing buttons and the io area.
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

import java.awt.Frame;
import java.awt.Button;
import java.awt.Panel;
import java.awt.TextField;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import java.io.Reader;
import java.io.Writer;
import java.io.IOException;


public class StdioFrame
    extends Frame
    implements ActionListener
{
    public final static String id
        = "$Id$";


    private StdioArea _stdio;
    private Button    _cutB, _copyB, _pasteB;

    /***** interface ActionListener begin *****/
    
    public void actionPerformed(ActionEvent e) 
    {
        String arg = e.getActionCommand();
        
        if (arg.equals("Clear"))
        {
            _stdio.clear();
        }
        else if (arg.equals(_copyB.getActionCommand()))
        {
            _stdio.copy();
        }
        else if (arg.equals(_cutB.getActionCommand())) 
        {
            _stdio.cut();
        }
        else if (arg.equals(_pasteB.getActionCommand()))
        {
            _stdio.paste();
        }
        else if (arg.equals(_stdio.getActionCommand()))
        {
            _pasteB.setEnabled(_stdio.canPaste());

            boolean readEnable = _stdio.canCopyCut();

            _copyB.setEnabled(readEnable);
             _cutB.setEnabled(readEnable);
        }
    }

    /***** interface ActionListener end *****/

    /***** applet initialisation function begin *****/
     
    public StdioFrame()
    {
        super("StdioFrame");

        {
            Panel p = new Panel();

            Button clearB = new Button("Clear");
            clearB.addActionListener(this); 
            p.add(clearB);

            _copyB = new Button("Copy");
            _copyB.addActionListener(this);
            p.add(_copyB);

            _cutB = new Button("Cut");
            _cutB.addActionListener(this);
            p.add(_cutB);

            _pasteB = new Button("Paste");
            _pasteB.addActionListener(this);
            p.add(_pasteB);

            add(p, "South");
        }

        {
            _stdio = new StdioArea();
            _stdio.addActionListener(this);
            add(_stdio, "North");
        }
    }

    /***** applet initialisation function end *****/

    public Reader stdin () { return _stdio.stdin (); }
    public Writer stdout() { return _stdio.stdout(); }
}
