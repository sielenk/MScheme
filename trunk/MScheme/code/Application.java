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


final class CallContinuation
    extends Continuation
{
    public final static String id
        = "$Id$";

    private final List _arguments;

    CallContinuation(
        Registers state,
        List      arguments
    )
    {
        super(state);
        _arguments = arguments;
    }

    protected Code execute(Registers state, Value value)
        throws SchemeException
    {
        return value.toFunction().call(
            state,
            _arguments
        );
    }


    protected String debugString()
    {
        return "call[" + _arguments + "]";
    }
}


final class PushContinuation
    extends Continuation
{
    public final static String id
        = "$Id$";


    private final List     _done;
    private final CodeList _todo;


    PushContinuation(
        Registers state,
        List      done,
        CodeList  todo
    )
    {
        super(state);
        _done = done;
        _todo = todo;
    }

    protected Code execute(Registers state, Value value)
        throws SchemeException
    {
        return Application.prepareNext(
            state,
            ListFactory.prepend(value, _done),
            _todo
        );
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


    static Code prepareNext(
        Registers state,
        List      done,
        CodeList  todo
    )
    {
        CodeList newTail = todo.getTail();

        if (newTail.isEmpty())
        {
            new CallContinuation(
                state,
                done
            );
        }
        else
        {
            new PushContinuation(
                state,
                done,
                newTail
            );
        }

        return todo.getHead();
    }


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
        return prepareNext(
            state,
            Empty.create(),
            _permutedApplication
        );
    }


    public String toString()
    {
        return "app:<" + _permutedApplication.getReversed().toString() + '>';
    }
}
