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

package mscheme.values.functions;

import mscheme.exceptions.RuntimeError;
import mscheme.exceptions.TypeError;
import mscheme.machine.Registers;

import mscheme.util.Arity;

import mscheme.values.Function;
import mscheme.values.List;
import mscheme.values.ListFactory;
import mscheme.values.ValueTraits;


final class AppendHelper1
    extends Reducer
{
    public final static String CVS_ID
        = "$Id$";


    AppendHelper1(Object initial)
    {
        super(initial);
    }

    protected Object combine(Object fst, Object snd)
    {
        return ListFactory.createPair(fst, snd);
    }
}


final class AppendHelper2
    extends Reducer
{
    public final static String CVS_ID
        = "$Id$";


    final static AppendHelper2 INSTANCE
        = new AppendHelper2();

    private AppendHelper2()
    {
        super(ListFactory.create());
    }

    protected Object combine(Object fst, Object snd)
        throws RuntimeError, TypeError
    {
        return new AppendHelper1(snd).foldRight(ValueTraits.toList(fst));
    }
}


public final class AppendFunction
    extends Function
{
    public final static String CVS_ID
        = "$Id$";


    public final static AppendFunction INSTANCE
        = new AppendFunction();

    protected Arity getArity()
    {
        return Arity.atLeast(0);
    }

    public Object call(Registers state, List arguments)
        throws RuntimeError, TypeError
    {
        return AppendHelper2.INSTANCE.reduceRight(arguments);
    }
}
