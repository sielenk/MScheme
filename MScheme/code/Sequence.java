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

import MScheme.values.ScmBoolean;

import MScheme.syntax.SequenceTags;

import MScheme.exceptions.SymbolNotFoundException;
import MScheme.exceptions.UnexpectedSyntax;

import MScheme.machine.Continuation;
import MScheme.machine.Registers;


public final class Sequence
    implements Code, SequenceTags
{
    public final static String id
        = "$Id$";


    private final int    _tag;
    private final Code[] _sequence;
    private final int    _index;

    private Sequence(int tag, Code[] sequence, int index)
    {
        _tag      = tag;
        _sequence = sequence;
        _index    = index;
    }

    private static Code create(int tag, Code[] sequence, int index)
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
            return new Sequence(tag, sequence, index);
        }
    }

    public static Code create(int tag, Code[] sequence)
    {
        if (sequence.length == 0)
        {
            return ScmBoolean.create(tag == TAG_AND);
        }
        else
        {
        	return create(tag, sequence, 0);
        }
    }

    public static Code create(Code[] sequence)
    {
    	return create(TAG_BEGIN, sequence);
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
                
                return
                    (
                        (
                            (_tag == TAG_AND)
                            &&
                            !value.isTrue()
                        )
                        ||
                        (
                            (_tag == TAG_OR)
                            &&
                            value.isTrue()
                        )
                    )
                    ? value.getLiteral()
                    : create(_tag, _sequence, _index + 1);
            }

            protected String debugString()
            {
                String prefix = "error";

                switch (_tag)
                {
                case TAG_BEGIN:
                    prefix = "seqence";
                    break;

                case TAG_AND:
                    prefix = "conjuction";
                    break;

                case TAG_OR:
                    prefix = "disjuction";
                    break;
                }

                return prefix + ":" + CodeArray.printTuple(
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
        CodeArray.force(_sequence);
        return this;
    }

    public String toString()
    {
        String prefix = "error";

        switch (_tag)
        {
        case TAG_BEGIN:
            prefix = "seq";
            break;

        case TAG_AND:
            prefix = "and";
            break;

        case TAG_OR:
            prefix = "or";
            break;
        }
        return prefix + ":<" + CodeArray.printTuple(_sequence) + '>';
    }
}
