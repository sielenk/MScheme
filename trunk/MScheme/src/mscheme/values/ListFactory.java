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


public abstract class ListFactory
{
    public final static String CVS_ID
        = "$Id$";


    // *** List creation ***

	public static IList prepend(Object head, IList tail) {
		return PairOrList.prepend(head, tail);
	}

	public static IList prependConst(Object head, IList tail) {
		return PairOrList.prependConst(head, tail);
	}

    public static IList create()
    {
        return Empty.INSTANCE;
    }

    public static IList createConst()
    {
        return Empty.INSTANCE;
    }

    public static IList create(Object first)
    {
        return prepend(first, create());
    }

    public static IList createConst(Object first)
    {
        return prependConst(first, createConst());
    }

    public static IList create(Object first, Object second)
    {
        return prepend(first, create(second));
    }

    public static IList createConst(Object first, Object second)
    {
        return prependConst(first, createConst(second));
    }

    public static IList create(Object first, Object second, Object third)
    {
        return prepend(first, create(second, third));
    }

    public static IList create(
		Object first,
		Object second,
		Object third,
		Object fourth)
    {
        return prepend(first, create(second, third, fourth));
    }


    // *** Pair creation ***
    
    public static IPair createPair(Object fst, Object snd)
    {
        return PairOrList.create(fst, snd);
    }
 
    public static IPair createConstPair(Object fst, Object snd)
    {
        return PairOrList.createConst(fst, snd);
    }
}

