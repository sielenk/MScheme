/* An Y-combinator to create recursive functions without define.
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

package mscheme.values.functions;

import mscheme.Code;
import mscheme.Value;

import mscheme.exceptions.SchemeException;
import mscheme.exceptions.TypeError;

import mscheme.machine.Registers;

import mscheme.values.Function;
import mscheme.values.List;
import mscheme.values.ListFactory;


class YWrappedFunction
            extends Function
{
    public final static String id
        = "$Id$";


    private final Function _f;

    YWrappedFunction(Function f)
    {
        _f = f;
    }

    public Code call(Registers state, List arguments)
        throws SchemeException
    {
        return _f.call(
                   state,
                   ListFactory.prepend(
                       this,
                       arguments
                   )
               );
    }
}


public final class YCombinator
            extends UnaryValueFunction
{
    public final static String id
        = "$Id$";


    public final static YCombinator INSTANCE = new YCombinator();

    protected Value checkedCall(Value fst)
        throws TypeError
    {
        return new YWrappedFunction(fst.toFunction());
    }
}
