/* A partial implementation of Code which invokes a waiting Continuation.
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

package MScheme.machine;

import MScheme.Value;
import MScheme.Code;

import MScheme.exceptions.SchemeException;
import MScheme.exceptions.RuntimeError;


/**
 * This class is the abstract base for return instructions
 * of the scheme machine.
 */
public abstract class Result
    implements Code
{
    /** The CVS id of the file containing this class. */
    public final static String id
        = "$Id$";


    /** The default constructor. */
    protected Result()
    { }


    /**
     * Returns the result of an evaluation.
     * <p>
     * @param  state  the current state of the scheme machine.
     */
    protected abstract Value getValue(Registers state)
    throws RuntimeError;


    /**
     * Passes an evaluation result to the current continuation.
     * <p>
     * @param  state  the current state of the scheme machine.
     * @return the code returned by
     *         {@link Continuation#invoke(Registers, Value)}.
     */
    public final Code executionStep(Registers state)
    throws SchemeException
    {
        return state.getContinuation().invoke(
                   state,
                   getValue(state)
               );
    }
}
