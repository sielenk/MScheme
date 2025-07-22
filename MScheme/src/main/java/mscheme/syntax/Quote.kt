/* The translation function for Scheme's 'quote'.
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
package mscheme.syntax

import mscheme.environment.StaticEnvironment
import mscheme.exceptions.TypeError
import mscheme.util.Arity.Companion.exactly
import mscheme.values.IList
import mscheme.values.ValueTraits.getConst


// *** quote ***
internal object Quote : CheckedTranslator(exactly(1)) {
    @Throws(TypeError::class)
    override fun checkedTranslate(
        compilationEnv: StaticEnvironment,
        arguments: IList
    ): Any? =
        getConst(arguments.head)
}
