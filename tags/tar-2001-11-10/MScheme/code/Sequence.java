/* The implementation of Scheme's 'begin'.
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

import MScheme.machine.Continuation;
import MScheme.machine.Registers;


final class SequenceContinuation
    extends Continuation
{
    public final static String id
    = "$Id$";


    private final CodeList _todo;

    private SequenceContinuation(
        Registers state,
        CodeList  todo
    )
    {
        super(state);
        _todo = todo;
    }

    static Code prepareNext(
        Registers state,
        CodeList  todo
    )
    {
        CodeList tail = todo.getTail();

        if (!tail.isEmpty())
        {
            new SequenceContinuation(state, tail);
        }

        return todo.getHead();
    }

    protected Code execute(Registers state, Value value)
    {
        return prepareNext(state, _todo);
    }


    protected String debugString()
    {
        return "seqence[" + _todo + "]";
    }
}


public final class Sequence
    implements Code
{
    public final static String id
    = "$Id$";


    private final CodeList _sequence;

    private Sequence(CodeList sequence)
    {
        _sequence = sequence;
    }

    public static Code create(CodeList sequence)
    {
        if (sequence.getTail().isEmpty())
        {
            return sequence.getHead();
        }
        else
        {
            return new Sequence(sequence);
        }
    }

    public Code executionStep(Registers state)
    {
        return SequenceContinuation.prepareNext(state, _sequence);
    }


    public String toString()
    {
        return "SEQ[" + _sequence.toString() + ']';
    }
}
