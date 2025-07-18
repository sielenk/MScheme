/* A base class for 'memq', 'memv' and 'member'.
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
package mscheme.values.functions

import mscheme.exceptions.ListExpected
import mscheme.exceptions.PairExpected
import mscheme.values.ValueTraits
import mscheme.values.ValueTraits.toList

internal abstract class MemberBase : BinaryValueFunction() {
    protected abstract fun equal(fst: Any?, snd: Any?): Boolean

    @Throws(ListExpected::class, PairExpected::class)
    override fun checkedCall(fst: Any?, snd: Any?): Any? {
        var tail = toList(snd)

        while (!tail.isEmpty) {
            if (equal(fst, tail.head)) {
                return tail
            }

            tail = tail.tail
        }

        return ValueTraits.FALSE
    }
}
