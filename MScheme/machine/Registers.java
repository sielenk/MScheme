/* The state of the scheme machine.
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

import MScheme.environment.DynamicEnvironment;

import MScheme.values.functions.UnaryFunction;


/**
 * The state of a scheme machine.
 */
public class Registers
{
    /** The CVS id of the file containing this class. */
    public final static String id
    = "$Id$";


    private Continuation       _continuation;
    private DynamicEnvironment _environment;


    /** */
    Registers(DynamicEnvironment environment)
    {
        _continuation = null;
        _environment  = environment;
    }

    /** Creates a copy of other. */
    Registers(final Registers other)
    {
        _continuation = other._continuation;
        _environment  = other._environment;
    }

    /** This is an assignment operator. */
    void assign(final Registers other)
    {
        _continuation = other._continuation;
        _environment  = other._environment;
    }

    void setContinuation(Continuation newContinuation)
    {
        _continuation = newContinuation;
    }

    Continuation getContinuation()
    {
        return _continuation;
    }


    /** Returns the current continuation value. */
    public UnaryFunction getCurrentContinuation()
    {
        return new ContinuationFunction(_continuation);
    }

    public UnaryFunction getComputationController()
    {
        return Controller.create(this);
    }

    /** Returns the current environment. */
    public DynamicEnvironment getEnvironment()
    {
        return _environment;
    }

    /** Changes the current environment. */
    public void setEnvironment(DynamicEnvironment newEnvironment)
    {
        _environment = newEnvironment;
    }
}

