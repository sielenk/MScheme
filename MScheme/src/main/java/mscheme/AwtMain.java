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

package mscheme;

import java.awt.Frame;

public final class AwtMain
{
    public final static String CVS_ID = "$Id$";

    private Frame _frame = null; //  @jve:decl-index=0:visual-constraint="14,13"

    private MSchemePanel _panel = null;

    private Frame get_frame()
    {
        if (_frame == null)
        {
            _frame = new Frame();
            _frame.setTitle("MScheme");
            _frame.setSize(600, 400);
            _frame.add(get_panel(), java.awt.BorderLayout.CENTER);
            _frame.addWindowListener(new java.awt.event.WindowAdapter()
            {   
            	public void windowClosing(java.awt.event.WindowEvent e) {    
            		get_panel().stop();
            		get_frame().setVisible(false);
                    get_frame().dispose();
            	}
                public void windowClosed(java.awt.event.WindowEvent e)
                {
                    System.exit(0);
                }
            });
        }
        return _frame;
    }

    private MSchemePanel get_panel()
    {
        if (_panel == null)
        {
            _panel = new MSchemePanel();
        }
        return _panel;
    }

    public static void main(String argv[])
            throws Exception
    {
        new AwtMain().get_frame().setVisible(true);
    }
}