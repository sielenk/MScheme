/* The interface required for compiled Scheme code.
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

import mscheme.code.IForceable;
import mscheme.exceptions.CompileError;


/**
 * The abstract base class for scheme machine instructions.
 * Opcode driven machines have to contain all the logic 
 * necessary to execute every single opcode. The scheme
 * machine implemented here only knows about this class.
 * The logic needed by the different instructions is contained
 * in the classes which implement them.
 */
public final class ICode
{
    /** The CVS id of the file containing this class. */
    String id
        = "$Id$";


    /**
     * Does the final Symbol lookup.
     */
    public static Object force(Object o)
        throws CompileError
    {
    	return
    		(o instanceof IForceable)
    		? ((IForceable)o).force()
    		: o;
    }
}
