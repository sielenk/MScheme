/* A base class for 'assq', 'assv' and 'assoc'.
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

package MScheme.values.functions;

import MScheme.Value;

import MScheme.exceptions.ListExpected;
import MScheme.exceptions.PairExpected;

import MScheme.values.List;
import MScheme.values.Pair;
import MScheme.values.ScmBoolean;

abstract class AssocBase
            extends BinaryValueFunction
{
    public final static String id
    = "$Id$";


    protected abstract boolean equal(Value fst, Value snd);

    protected final Value checkedCall(
        Value key,
        Value values
    ) throws ListExpected, PairExpected
    {
        for (
            List tail = values.toList();
            !tail.isEmpty();
            tail = tail.getTail()
        )
        {
            Pair pair = tail.getHead().toPair();

            if (equal(key, pair.getFirst()))
            {
                return pair;
            }
        }

        return ScmBoolean.createFalse();
    }
}
