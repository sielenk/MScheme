/* A list factory.
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

package mscheme.values;

import mscheme.Value;


public abstract class ListFactory
{
    public final static String CVS_ID
        = "$Id$";


    // *** List creation ***

	public static List prepend(Object head, List tail) {
		return PairOrList.prepend(head, tail);
	}

    public static List create()
    {
        return Empty.create();
    }

    public static List create(Object first)
    {
        return prepend(first, create());
    }

    public static List create(Object first, Object second)
    {
        return prepend(first, create(second));
    }

    public static List create(Object first, Object second, Object third)
    {
        return prepend(first, create(second, third));
    }

    public static List create(
		Object first,
		Object second,
		Object third,
		Object fourth)
    {
        return prepend(first, create(second, third, fourth));
    }


    // *** Pair creation ***
    
    public static Pair createPair(Object fst, Object snd)
    {
        return PairOrList.create(fst, snd);
    }
 
    public static Pair createConstPair(Value fst, Value snd)
    {
        return PairOrList.createConst(fst, snd);
    }
}

