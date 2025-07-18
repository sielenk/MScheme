/* The 'append' function.
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

import mscheme.exceptions.RuntimeError
import mscheme.exceptions.TypeError
import mscheme.machine.Registers
import mscheme.values.Function
import mscheme.values.IList
import mscheme.values.ListFactory
import mscheme.values.ValueTraits

internal class AppendHelper1(initial: Any?) : Reducer(initial) {
    override fun combine(fst: Any?, snd: Any?): Any =
        ListFactory.createPair(fst, snd)
}

internal object AppendHelper2 : Reducer(ListFactory.create()) {
    @Throws(RuntimeError::class, TypeError::class)
    override fun combine(fst: Any?, snd: Any?): Any? =
        AppendHelper1(snd).foldRight(ValueTraits.toList(fst))
}

object AppendFunction : Function() {
    @Throws(RuntimeError::class, TypeError::class)
    override fun call(state: Registers, arguments: IList): Any? =
        AppendHelper2.reduceRight(arguments)
}
