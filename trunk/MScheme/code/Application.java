/* The implementation of Scheme's function call.
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

package MScheme.code;

import MScheme.Value;
import MScheme.Code;

import MScheme.machine.Registers;
import MScheme.machine.Continuation;

import MScheme.values.ListFactory;
import MScheme.values.List;
import MScheme.values.Empty;

import MScheme.exceptions.*;


final class PushContinuation
    extends Continuation
{
    public final static String id
        = "$Id$";


    private final List     _done;
    private final CodeList _todo;


    private PushContinuation(
        Registers state,
        List      done,
        CodeList  todo
    )
    {
        super(state);
        _done = done;
        _todo = todo;
    }

    static Code prepareNext(
        Registers state,
        List      done,
        CodeList  todo
    )
    {
        new PushContinuation(
            state,
            done,
            todo.getTail()
        );

        return todo.getHead();
    }

    protected Code execute(Registers state, Value value)
        throws SchemeException
    {
        if (_todo.isEmpty())
        {
            return value.toFunction().call(state, _done);
        }
        else
        {
            return prepareNext(
                       state,
                       ListFactory.prepend(value, _done),
                       _todo
                   );
        }
    }


    protected String debugString()
    {
        return "push[" + _todo.getReversed() + " | " + _done + "]";
    }
}


public final class Application
    implements Code
{
    public final static String id
        = "$Id$";


    private final CodeList _permutedApplication;

    private Application(CodeList application)
    {
        _permutedApplication = application.getReversed();
    }

    public static Code create(CodeList application)
    {
        return new Application(application);
    }

    public Code executionStep(Registers state)
    {
        return PushContinuation.prepareNext(
                   state,
                   Empty.create(),
                   _permutedApplication
               );
    }


    public String toString()
    {
        return "APPLY[" + _permutedApplication.getReversed().toString() + ']';
    }
}
