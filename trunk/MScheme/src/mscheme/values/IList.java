/* An extended interface for lists.
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

package mscheme.values;

import mscheme.environment.StaticEnvironment;

import mscheme.exceptions.ListExpected;
import mscheme.exceptions.PairExpected;
import mscheme.exceptions.SchemeException;


public interface IList
{
    /** The CVS id of the file containing this class. */
    String CVS_ID
        = "$Id$";


    boolean isValid();
	IList validate()  throws ListExpected;

    boolean isEmpty();

    int     getLength  ();
    IList  getReversed();
    IList  getCopy    ();
    Object getHead    () throws PairExpected;
    IList  getTail    () throws PairExpected;

    Object[] getCompiledArray(StaticEnvironment compilationEnv)
        throws SchemeException;

    Object[] getCompiledArray(StaticEnvironment compilationEnv, int index)
        throws SchemeException;

	Object[] getArray(         );
	Object[] getArray(int index);
}
