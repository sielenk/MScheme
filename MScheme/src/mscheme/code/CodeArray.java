/* A helper class for sequences and applications.
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

package mscheme.code;

import mscheme.compiler.Compiler;
import mscheme.exceptions.CompileError;


class CodeArray
{
    public final static String CVS_ID
        = "$Id$";


    static String printTuple(
        Object[] tuple
    )
    {
        return printTuple(tuple, 0, tuple.length);
    }

    static String printTuple(
        Object[] tuple,
        int      begin,
        int      end
    )
    {
        StringBuffer buffer = new StringBuffer();
        
        buffer.append('<');
        for (int i = begin;;)
        {
            buffer.append(
                tuple[i].toString()
            );
            if (++i == end) {
                break;
            }
            buffer.append(", ");
        }
        buffer.append('>');

        return buffer.toString(); 
    }

    static void force(Object[] array)
        throws CompileError
    {
        for (int i = 0; i < array.length; ++i)
        {
            array[i] = Compiler.force(array[i]);
        }
    }
}
