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

package mscheme.code;

import mscheme.compiler.IForceable;
import mscheme.exceptions.CompileError;
import mscheme.machine.IContinuation;
import mscheme.machine.Registers;
import mscheme.syntax.ISequenceTags;
import mscheme.values.ValueTraits;


public final class Sequence
    implements ISequenceTags, IForceable, IReduceable
{
	public final static String CVS_ID
        = "$Id$";


    private final int      _tag;
    private final Object[] _sequence;

    private Sequence(int tag, Object[] sequence)
    {
        _tag      = tag;
        _sequence = sequence;
    }

    public static Object create(int tag, Object[] sequence)
    {
        switch (sequence.length)
        {
        case 0:
            return tag == TAG_AND;

        case 1:
            return sequence[0];

        default:
            return new Sequence(tag, sequence);
        }
    }


    public static Object create(Object[] sequence)
    {
        return create(TAG_BEGIN, sequence);
    }

	public static Object createConj(Object[] sequence)
	{
		return create(TAG_AND, sequence);
	}

	public static Object createDisj(Object[] sequence)
	{
		return create(TAG_OR, sequence);
	}


    public Object force()
        throws CompileError
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




	public Object reduce(Registers registers)
	{
		return prepareNext(registers, 0);
	}

	private Object prepareNext(
		Registers          registers,
		final int          index)
	{
		if (index + 1 < _sequence.length)
		{
			registers.push(
			    new IContinuation()
			    {
					@SuppressWarnings("unused")
                    public final static String CVS_ID
						= "$Id$";
 
 
					public Object invoke(
						Registers registers,
						Object    value)
					{
						if (
							((_tag == TAG_AND) && !ValueTraits.isTrue(value))
							||
							((_tag == TAG_OR ) &&  ValueTraits.isTrue(value))
						)
						{
							return value;
						}
						else
						{
							return prepareNext(registers, index + 1);
						}
					}
				});
		}

		return _sequence[index];
	}
}
