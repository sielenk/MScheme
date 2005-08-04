/* The interface required for for Scheme values.
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

package mscheme;

import mscheme.environment.StaticEnvironment;

import mscheme.exceptions.ListExpected;
import mscheme.exceptions.PairExpected;
import mscheme.exceptions.SchemeException;

import mscheme.values.List;
import mscheme.values.Pair;


public interface Value
{
    /** The CVS id of the file containing this class. */
    String id
        = "$Id$";

    /**
     * Returns an immutable version of <code>this</code>.
     */
    Object getConst();


    /**
     * Returns <code>true</code> for proper lists.
     */
    boolean isList();


    // disjoint types (as in R5RS, 3.2)


    // type casts (to get SchemeExceptions instead of ClassCastExceptions)

    /**
     * Casts a reference to {@link Value} to a reference to {@link List}.
     */
    List toList() throws ListExpected;

    /**
     * Casts a reference to {@link Value} to a reference to {@link Pair}.
     */
    Pair toPair() throws PairExpected;


    // the java equivalence predicate

    /**
     * Implements Java's <code>equals</code>-equality in terms of
     * Scheme's <code>equal</code>-equality.
     * <p>
     * @param other the object with which to compare.
     * @return <code>(other instanceof Value) && equal((Value)other)</code>
     */
    boolean equals(Object other);


    // compilation functions

    /**
     * Compiles a value as normal code.
     */
    Object getCompiled(StaticEnvironment compilationEnv)
        throws SchemeException;

    /**
     * Compiles a value as list head.
     * A symbol at the head of a list can be a syntactic
     * keyword which needs special treatment.
     */
    Syntax getSyntax(StaticEnvironment compilationEnv)
        throws SchemeException;
}