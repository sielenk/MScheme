/* Applet class for MScheme.
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

/* Created on 28.12.2004, 09:50:48 by sielenk */

package mscheme;

import java.applet.Applet;
import java.awt.BorderLayout;

/**
 * @author sielenk
 * 
 * TODO To change the template for this generated type comment go to Window -
 * Preferences - Java - Code Style - Code Templates
 */
public class AppletMain
        extends Applet
{
    public final static String CVS_ID = "$Id$";

    private MSchemePanel _panel = null; //  @jve:decl-index=0:visual-constraint="-132,-123"

    public AppletMain()
    {
        super();
    }

    public void init()
    {
        this.setLayout(new BorderLayout());
        this.add(get_panel(), java.awt.BorderLayout.CENTER);
    }

    public void stop()
    {
        get_panel().stop();
    }

    public void start()
    {
        get_panel().start();
    }

    /**
     * This method initializes stdioFrame1
     * 
     * @return mscheme.StdioFrame
     */
    private MSchemePanel get_panel()
    {
        if (_panel == null)
        {
            _panel = new MSchemePanel();
        }
        return _panel;
    }
} //  @jve:decl-index=0:visual-constraint="10,10"
