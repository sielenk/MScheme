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
package de.masitec.mscheme.values


object ListFactory {
    // *** List creation ***
    fun prepend(head: Any?, tail: IList): IList =
        PairOrList.prepend(head, tail)

    fun prependConst(head: Any?, tail: IList): IList =
        PairOrList.prependConst(head, tail)

    fun create(): IList =
        Empty

    fun createConst(): IList =
        Empty

    fun create(first: Any?): IList =
        prepend(first, create())

    fun createConst(first: Any?): IList =
        prependConst(first, createConst())

    fun create(first: Any?, second: Any?): IList =
        prepend(first, create(second))

    fun create(first: Any?, second: Any?, third: Any?): IList =
        prepend(first, create(second, third))

    fun create(
        first: Any?,
        second: Any?,
        third: Any?,
        fourth: Any?
    ): IList =
        prepend(
            first,
            create(second, third, fourth)
        )

    // *** Pair creation ***
    fun createPair(fst: Any?, snd: Any?): IMutablePair =
        PairOrList.create(fst, snd)

    fun createConstPair(fst: Any?, snd: Any?): IPair =
        PairOrList.createConst(fst, snd)

    fun createConst(first: Any?, second: Any?): IList =
        prependConst(first, createConst(second))
}
