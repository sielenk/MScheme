/* 'dynamic-wind' helper continuation.
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

import MScheme.code.Sequence;

import MScheme.exceptions.RuntimeError;
import MScheme.exceptions.TypeError;


public final class WindContinuation
            extends Continuation
{
    public final static String id
    = "$Id$";


    private final Code _before;
    private final Code _after;

    private WindContinuation(
        Registers state,
        Code      before,
        Code      after
    )
    {
        super(state);
        _before = before;
        _after  = after;
    }


    public static Code create(
        Registers state,
        Code      before,
        Code      thunk,
        Code      after
    ) throws RuntimeError, TypeError
    {
	Code[] seq = new Code[2];

        seq[0] = before;
        seq[1] = thunk;

        new WindContinuation(state, before, after);

        return Sequence.create(seq);
    }

    protected Code execute(
        Registers state,
        Value     result
    ) throws RuntimeError, TypeError
    {
	Code[] seq = new Code[2];

        seq[0] = _after;
        seq[1] = result.getLiteral();

        return Sequence.create(seq);
    }


    protected int dynamicWindLeave(Code[] sequence, int index)
    {
	sequence[index++] = _after;

	return super.dynamicWindLeave(
            sequence,
            index
        );
    }

    protected int dynamicWindEnter(Code[] sequence, int index)
    {
        index = super.dynamicWindLeave(
            sequence,
            index
        );

        sequence[--index] = _before;

        return index;
    }


    protected String debugString()
    {
        return "wind[" + _before + ", " + _after + "]";
    }
}
