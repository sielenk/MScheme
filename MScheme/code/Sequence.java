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

import MScheme.Code;
import MScheme.Value;

import MScheme.exceptions.SymbolNotFoundException;
import MScheme.exceptions.UnexpectedSyntax;

import MScheme.machine.Continuation;
import MScheme.machine.Registers;


public final class Sequence
    implements Code
{
    public final static String id
        = "$Id$";


    private final Code[] _sequence;
    private final int    _index;

    private Sequence(Code[] sequence, int index)
    {
        _sequence = sequence;
        _index    = index;
    }

    private static Code create(Code[] sequence, int index)
    {
        if (index + 1 == sequence.length)
        {
            // index denotes the last element
            // don't create a new Sequence object
            return sequence[index];
        }
        else
        {
            // There are at least two elements to process 
            // in sequence -> create a new one.
            return new Sequence(sequence, index);
        }
    }

    public static Code create(Code[] sequence)
    {
    	return create(sequence, 0);
    }

    public Code executionStep(Registers state)
    {
        new Continuation(state) {
            public final static String id
                = "$Id$";

 
            protected Code execute(Registers innerState, Value value)
            {
                // _index+1 will always be < sequence.length
                // this is enforced by create(Code[], int)
                return create(_sequence, _index + 1);
            }

            protected String debugString()
            {
                return "seqence:" + CodeArray.printTuple(
                    _sequence, 
                    _index + 1, 
                    _sequence.length
                );
            }
        };

        return _sequence[_index];
    }

    public Code force()
        throws SymbolNotFoundException, UnexpectedSyntax
    {
        return create(
            CodeArray.force(_sequence)
        );
    }

    public String toString()
    {
        return "seq:<" + CodeArray.printTuple(_sequence) + '>';
    }
}
