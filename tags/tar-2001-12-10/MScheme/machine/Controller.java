/* An implementation of subcontinuations.
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

import java.io.Writer;
import java.io.IOException;

import MScheme.Value;
import MScheme.Code;

import MScheme.code.Application;

import MScheme.values.functions.UnaryFunction;

import MScheme.exceptions.SchemeException;
import MScheme.exceptions.RuntimeError;


final class Subcontinuation
    extends UnaryFunction
{
    /** The CVS id of the file containing this class. */
    public final static String id
        = "$Id$";


    private final Continuation _leaf;

    Subcontinuation(
        Continuation rootsParent,
        Continuation leaf
    )
    {
        _leaf = Continuation.copyAndPrependSubcontinuation(
            rootsParent,
            leaf,
            null
        );
    }


    // implementation of Value

    public void write(Writer destination)
        throws IOException
    {
        destination.write(
            "#[subcontinuation]"
        );
    }


    // implementation of UnaryFunction

    protected Code checkedCall(Registers state, Value argument)
    {
        Continuation target = Continuation.copyAndPrependSubcontinuation(
            null,
            _leaf,
            state.getContinuation()
        );

        return new ContinuationFunction(
            target
        ).checkedCall(
            state,
            argument
        );
    }
}


final class RootContinuation
    extends Continuation
{
    private Object _equalityTag;


    Continuation cloneContinuation()
    {
        RootContinuation clonedRoot
            = (RootContinuation)super.cloneContinuation();

        clonedRoot._equalityTag = _equalityTag;
        return clonedRoot;
    }

    RootContinuation(Registers state)
    {
        super(state);
        _equalityTag = new Object();
    }

    protected Code execute(Registers state, Value result)
        throws SchemeException
    {
        return result.getLiteral();
    }

    protected String debugString()
    {
        return "spawn root";
    }


    public boolean equals(Object other)
    {
        try {
            return _equalityTag == ((RootContinuation)other)._equalityTag;
        }
        catch (ClassCastException e)
        { }
        
        return false;
    }
}


public final class Controller
    extends UnaryFunction
{
    /** The CVS id of the file containing this class. */
    public final static String id
        = "$Id$";


    private final RootContinuation _root;

    private Controller(RootContinuation root)
    {
        _root = root;
    }

    static Controller create(Registers state)
    {
        return new Controller(
            new RootContinuation(state)
        );
    }

    // implementation of Value

    public void write(Writer destination)
        throws IOException
    {
        destination.write(
            "#[subcomputation controller]"
        );
    }


    // implementation of UnaryFunction

    protected Code checkedCall(Registers state, Value argument)
        throws RuntimeError
    {
        Continuation current = state.getContinuation();
        while (!current.equals(_root))
        {
            current = current.getParent();

            if (current == null)
            {
                throw new RuntimeError(
                    this,
                    "invalid subcomputation controller use"
                );
            }
        }

        Code[] application = new Code[2];

        application[0] = argument.getLiteral();
        application[1] = new ContinuationFunction(
                _root.getParent()
            ).checkedCall(
                state,
                new Subcontinuation(  
                    _root.getParent(),
                    state.getContinuation()
                )
            );

        return Application.create(application);
    }
}
