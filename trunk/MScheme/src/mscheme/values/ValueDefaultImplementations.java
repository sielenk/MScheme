/* A common base class for Scheme values. It implements most of
   the methods required by the interface Value.
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

import mscheme.Syntax;
import mscheme.Value;

import mscheme.environment.StaticEnvironment;

import mscheme.exceptions.ListExpected;
import mscheme.exceptions.PairExpected;
import mscheme.exceptions.SchemeException;

import mscheme.syntax.ProcedureCall;


public abstract class ValueDefaultImplementations
    implements Value, Comparable
{
    /** The CVS id of the file containing this class. */
    public final static String CVS_ID
        = "$Id$";


    /** The default constructor. */
    protected ValueDefaultImplementations()
    { }


    /**
     * @return <code>this</code>.
     */
    public Object getConst()
    {
        return this;
    }

    /**
     * @return <code>false</code>
     */
    public boolean isList()
    {
        return false;
    }


    // disjoint types (as in R5RS, 3.2)

    /**
     * @return <code>false</code>
     */
    public boolean isPair()
    {
        return false;
    }

    // type casts (to get SchemeExceptions instead of ClassCastExceptions)

    /**
     * @throws ListExpected
     */
    public List toList()
        throws ListExpected
    {
        throw new ListExpected(this);
    }

    /**
     * @throws PairExpected
     */
    public Pair toPair()
        throws PairExpected
    {
        throw new PairExpected(this);
    }


    // scheme equivalence predicates (as in R5RS, 6.1)

    /**
     * @return <code>this == other</code>
     */
    public boolean eq(Object other)
    {
        return this == other;
    }

    /**
     * @return <code>eq(other)</code>
     */
    public boolean eqv(Object other)
    {
        return eq(other);
    }

    /**
     * @return <code>eqv(other)</code>
     */
    public boolean equal(Object other)
    {
        return eqv(other);
    }


    // the java equivalence predicate

    /**
     * @return <code>(other instanceof Value) && equal((Value)other)</code>
     */
    public final boolean equals(Object other)
    {
        return (other instanceof Value) && equal((Value)other);
    }


    // compilation functions

    /**
     * Assumes the value to be a constant.
     * <p>
     * @param  compilationEnv ignored by this implementation
     * @return {@link #getConst()}
     */
    public Object getCompiled(StaticEnvironment compilationEnv)
        throws SchemeException
    {
        compilationEnv.setStateClosed();
        return getConst();
    }

    /**
     * Compiles as normal code.
     * <p>
     * @return <code>ProcedureCall.create(this)</code>
     */
    public Syntax getSyntax(StaticEnvironment compilationEnv)
        throws SchemeException
    {
        return ProcedureCall.create(this);
    }
}
