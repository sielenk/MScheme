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

import java.io.IOException;
import java.io.Writer;

import mscheme.environment.Environment;
import mscheme.environment.StaticEnvironment;

import mscheme.exceptions.CharExpected;
import mscheme.exceptions.EnvironmentExpected;
import mscheme.exceptions.FunctionExpected;
import mscheme.exceptions.InputPortExpected;
import mscheme.exceptions.ListExpected;
import mscheme.exceptions.NumberExpected;
import mscheme.exceptions.OutputPortExpected;
import mscheme.exceptions.PairExpected;
import mscheme.exceptions.SchemeException;
import mscheme.exceptions.StringExpected;
import mscheme.exceptions.SymbolExpected;
import mscheme.exceptions.VectorExpected;

import mscheme.values.List;
import mscheme.values.Pair;
import mscheme.values.Symbol;
import mscheme.values.ScmNumber;
import mscheme.values.ScmChar;
import mscheme.values.ScmString;
import mscheme.values.ScmVector;
import mscheme.values.InputPort;
import mscheme.values.OutputPort;
import mscheme.values.Function;


public interface Value
    extends Code
{
    /** The CVS id of the file containing this class. */
    String id
        = "$Id$";

    /**
     * Returns an immutable version of <code>this</code>.
     */
    Value getConst();

    /**
     * Returns Scheme's boolean interpretation of a value.
     * All values except <code>#f</code> are regarded as
     * true in Scheme.
     */
    boolean isTrue();


    /**
     * Returns <code>true</code> for proper lists.
     */
    boolean isList();


    // disjoint types (as in R5RS, 3.2)

    /**
     * Returns <code>true</code> for instances of 
     * {@link ScmBoolean}.
     */
    boolean isScmBoolean();

    /**
     * Returns <code>true</code> for instances of 
     * {@link Pair}.
     */
    boolean isPair();

    /**
     * Returns <code>true</code> for instances of 
     * {@link Symbol}.
     */
    boolean isSymbol();

    /**
     * Returns <code>true</code> for instances of 
     * {@link ScmNumber}.
     */
    boolean isScmNumber();

    /**
     * Returns <code>true</code> for instances of 
     * {@link ScmChar}.
     */
    boolean isScmChar();

    /**
     * Returns <code>true</code> for instances of 
     * {@link ScmString}.
     */
    boolean isScmString();

    /**
     * Returns <code>true</code> for instances of 
     * {@link ScmVector}.
     */
    boolean isScmVector();

    /**
     * Returns <code>true</code> for instances of 
     * {@link OutputPort} and {@link InputPort}.
     */
    boolean isPort();

    /**
     * Returns <code>true</code> for instances of 
     * {@link Function}.
     */
    boolean isFunction();


    // type casts (to get SchemeExceptions instead of ClassCastExceptions)

    /**
     * Casts a reference to {@link Value} to a reference to {@link List}.
     */
    List toList() throws ListExpected;

    /**
     * Casts a reference to {@link Value} to a reference to {@link Pair}.
     */
    Pair toPair() throws PairExpected;

    /**
     * Casts a reference to {@link Value} to a reference to {@link Symbol}.
     */
    Symbol toSymbol() throws SymbolExpected;

    /**
     * Casts a reference to {@link Value} to a reference to {@link ScmNumber}.
     */
    ScmNumber toScmNumber() throws NumberExpected;

    /**
     * Casts a reference to {@link Value} to a reference to {@link ScmChar}.
     */
    ScmChar toScmChar() throws CharExpected;

    /**
     * Casts a reference to {@link Value} to a reference to {@link ScmString}.
     */
    ScmString toScmString() throws StringExpected;

    /**
     * Casts a reference to {@link Value} to a reference to {@link ScmVector}.
     */
    ScmVector toScmVector() throws VectorExpected;

    /**
     * Casts a reference to {@link Value} to a reference to {@link InputPort}.
     */
    InputPort toInputPort() throws InputPortExpected;

    /**
     * Casts a reference to {@link Value} to a reference to {@link OutputPort}.
     */
    OutputPort toOutputPort() throws OutputPortExpected;

    /**
     * Casts a reference to {@link Value} to a reference to {@link Function}.
     */
    Function toFunction() throws FunctionExpected;

    /**
     * Casts a reference to {@link Value} to a reference to {@link Environment}.
     */
    Environment toEnvironment() throws EnvironmentExpected;

    /**
     * Casts a reference to {@link Value} to a reference to {@link StaticEnvironment}.
     */
    StaticEnvironment toStaticEnvironment() throws EnvironmentExpected;


    // scheme equivalence predicates (as in R5RS, 6.1)

    /**
     * Compares two values for Scheme's <code>eq</code>-equality.
     * This is the most discriminating equality in Scheme and is
     * supposed to be easiest to check.
     * <p>
     * @param other the value with which to compare.
     */
    boolean eq(Value other);

    /**
     * Compares two values for Scheme's <code>eqv</code>-equality.
     * This equality is supposed to compare characters and numbers
     * of the same value as equal, even if they are not the same
     * instance.
     * <p>
     * @param other the value with which to compare.
     */
    boolean eqv(Value other);

    /**
     * Compares two values for Scheme's <code>equal</code>-equality.
     * This equality compares compound values recursively, primitive
     * values like <code>eqv</code>.
     * <p>
     * @param other the value with which to compare.
     */
    boolean equal(Value other);


    // the java equivalence predicate

    /**
     * Implements Java's <code>equals</code>-equality in terms of
     * Scheme's <code>equal</code>-equality.
     * <p>
     * @param other the object with which to compare.
     * @return <code>(other instanceof Value) && equal((Value)other)</code>
     */
    boolean equals(Object other);


    // scheme io functions

    /**
     * Part of Scheme's <code>write</code> function implementation.
     * <p>
     * @param  destination  the java writer which expects the output.
     */
    void write(Writer destination) throws IOException;

    /**
     * Part of Scheme's <code>display</code> function implementation.
     * <p>
     * @param  destination  the java writer which expects the output.
     */
    void display(Writer destination) throws IOException;


    // the java io function

    /**
     * Implements Java's <code>toString</code> in terms of
     * Scheme's <code>write</code>.
     * <p>
     * @return a java string representation of the value.
     */
    String toString();


    // compilation functions

    /**
     * Compiles a value as normal code.
     */
    Code getCompiled(StaticEnvironment compilationEnv)
        throws SchemeException;

    /**
     * Compiles a value as list head.
     * A symbol at the head of a list can be a syntactic
     * keyword which needs special treatment.
     */
    Syntax getSyntax(StaticEnvironment compilationEnv)
        throws SchemeException;
}
