/* The implementation of Scheme's booleans.
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

package MScheme.values;

import java.io.Writer;
import java.io.IOException;

import MScheme.Value;


public final class ScmBoolean
            extends ValueDefaultImplementations
{
    public final static String id
    = "$Id$";


    private final static ScmBoolean TRUE  = new ScmBoolean();
    private final static ScmBoolean FALSE = new ScmBoolean();

    private ScmBoolean()
    { }


    public static ScmBoolean create(boolean flag)
    {
        return flag ? TRUE : FALSE;
    }

    public static ScmBoolean createFalse()
    {
        return FALSE;
    }

    public static ScmBoolean createTrue()
    {
        return TRUE;
    }


    public boolean isTrue()
    {
        return (this != FALSE);
    }

    public boolean isScmBoolean()
    {
        return true;
    }

    public void write(Writer destination)
    throws IOException
    {
        destination.write('#');
        if (isTrue())
        {
            destination.write('t');
        }
        else
        {
            destination.write('f');
        }
    }
}
