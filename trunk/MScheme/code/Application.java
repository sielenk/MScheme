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

import MScheme.Code;
import MScheme.Value;

import MScheme.exceptions.SchemeException;
import MScheme.exceptions.CompileError;

import MScheme.machine.Continuation;
import MScheme.machine.Registers;

import MScheme.values.Empty;
import MScheme.values.List;
import MScheme.values.ListFactory;


public final class Application
    implements Code
{
    public final static String id
        = "$Id$";


    private Code prepareNext(
        Registers  state,
        final List done,
        final int  index
    )
    {
        if (index == 0)
        {
            ApplyContinuation.create(
                state,
                done
            );
        }
        else
        {
            new Continuation(state)
            {
                public final static String id
                    = "$Id$";


                private final List _done  = done;
                private final int  _index = index;

                protected Code executionStep(Registers innerState, Value value)
                    throws SchemeException
                {
                    return prepareNext(
                        innerState,
                        ListFactory.prepend(value, _done),
                        _index - 1
                    );
                }


                protected String debugString()
                {
                    return 
                        "push:<"
                        + CodeArray.printTuple(
                              _application, 0, index
                          )
                        + ">, " + _done + '>';
                }
            };
        }

        return _application[index];
    }


    private final Code[] _application;

    private Application(Code[] application)
    {
        _application = application;
    }

    public static Code create(Code[] application)
    {
        return new Application(application);
    }

    public Code executionStep(Registers state)
    {
        return prepareNext(
            state,
            Empty.create(),
            _application.length - 1
        );
    }

    public Code force()
        throws CompileError
    {
        CodeArray.force(_application);
        return this;
    }

    public String toString()
    {
        return "app:" + CodeArray.printTuple(_application);
    }
}
