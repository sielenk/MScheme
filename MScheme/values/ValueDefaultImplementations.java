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

package MScheme.values;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;

import MScheme.Code;
import MScheme.Syntax;
import MScheme.Value;

import MScheme.environment.Environment;
import MScheme.environment.StaticEnvironment;

import MScheme.exceptions.CharExpected;
import MScheme.exceptions.EnvironmentExpected;
import MScheme.exceptions.FunctionExpected;
import MScheme.exceptions.InputPortExpected;
import MScheme.exceptions.ListExpected;
import MScheme.exceptions.NumberExpected;
import MScheme.exceptions.OutputPortExpected;
import MScheme.exceptions.PairExpected;
import MScheme.exceptions.SchemeException;
import MScheme.exceptions.StringExpected;
import MScheme.exceptions.SymbolExpected;
import MScheme.exceptions.VectorExpected;

import MScheme.machine.Result;
import MScheme.machine.Registers;

import MScheme.syntax.ProcedureCall;


public abstract class ValueDefaultImplementations
    extends    Result
    implements Value
{
    /** The CVS id of the file containing this class. */
    public final static String id
        = "$Id$";


    /** The default constructor. */
    protected ValueDefaultImplementations()
    { }


    protected final Value getValue(Registers state)
    {
        return this;
    }

    public Code force()
    {
        return this;
    }


    /**
     * @return <code>this</code>.
     */
    public Value getConst()
    {
        return this;
    }

    /**
     * @return <code>true</code>
     */
    public boolean isTrue()
    {
        return true;
    }


    /**
     * @return <code>false</code>
     */
    public boolean isList()
    {
        return false;
    }

    /**
     * @return <code>false</code>
     */
    public boolean isEmpty()
    {
        return false;
    }


    // disjoint types (as in R5RS, 3.2)

    /**
     * @return <code>false</code>
     */
    public boolean isScmBoolean()
    {
        return false;
    }

    /**
     * @return <code>false</code>
     */
    public boolean isPair()
    {
        return false;
    }

    /**
     * @return <code>false</code>
     */
    public boolean isSymbol()
    {
        return false;
    }

    /**
     * @return <code>false</code>
     */
    public boolean isScmNumber()
    {
        return false;
    }

    /**
     * @return <code>false</code>
     */
    public boolean isScmChar()
    {
        return false;
    }

    /**
     * @return <code>false</code>
     */
    public boolean isScmString()
    {
        return false;
    }

    /**
     * @return <code>false</code>
     */
    public boolean isScmVector()
    {
        return false;
    }

    /**
     * @return <code>false</code>
     */
    public boolean isPort()
    {
        return false;
    }

    /**
     * @return <code>false</code>
     */
    public boolean isFunction()
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

    /**
     * @throws SymbolExpected
     */
    public Symbol toSymbol()
    throws SymbolExpected
    {
        throw new SymbolExpected(this);
    }

    /**
     * @throws NumberExpected
     */
    public ScmNumber toScmNumber()
    throws NumberExpected
    {
        throw new NumberExpected(this);
    }

    /**
     * @throws CharExpected
     */
    public ScmChar toScmChar()
    throws CharExpected
    {
        throw new CharExpected(this);
    }

    /**
     * @throws StringExpected
     */
    public ScmString toScmString()
    throws StringExpected
    {
        throw new StringExpected(this);
    }

    /**
     * @throws VectorExpected
     */
    public ScmVector toScmVector()
    throws VectorExpected
    {
        throw new VectorExpected(this);
    }

    /**
     * @throws InputPortExpected
     */
    public InputPort toInputPort()
    throws InputPortExpected
    {
        throw new InputPortExpected(this);
    }

    /**
     * @throws OutputPortExpected
     */
    public OutputPort toOutputPort()
    throws OutputPortExpected
    {
        throw new OutputPortExpected(this);
    }

    /**
     * @throws FunctionExpected
     */
    public Function toFunction()
    throws FunctionExpected
    {
        throw new FunctionExpected(this);
    }

    /**
     * @throws EnvironmentExpected
     */
    public Environment toEnvironment()
    throws EnvironmentExpected
    {
        throw new EnvironmentExpected(this);
    }

    /**
     * @throws EnvironmentExpected
     */
    public StaticEnvironment toStaticEnvironment()
    throws EnvironmentExpected
    {
        throw new EnvironmentExpected(this);
    }


    // scheme equivalence predicates (as in R5RS, 6.1)

    /**
     * @return <code>this == other</code>
     */
    public boolean eq(Value other)
    {
        return this == other;
    }

    /**
     * @return <code>eq(other)</code>
     */
    public boolean eqv(Value other)
    {
        return eq(other);
    }

    /**
     * @return <code>eqv(other)</code>
     */
    public boolean equal(Value other)
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


    // scheme io functions

    /**
     * Calls {@link #write write(destination)}.
     */
    public void display(Writer destination)
    throws IOException
    {
        write(destination);
    }


    // the java io function

    /**
     * Implements Java's <code>toString</code> in terms of
     * Scheme's <code>write</code>.
     * <p>
     * @return a java string representation of the value.
     */
    public final String toString()
    {
        StringWriter out = new StringWriter();

        try
        {
            write(out);
        }
        catch (IOException e)
        { }

        return out.toString();
    }


    // compilation functions

    /**
     * @return a newly created {@link Literal}.
     */
    public final Code getLiteral()
    {
        return this;
    }

    /**
     * Assumes the value to be a constant.
     * <p>
     * @param  compilationEnv ignored by this implementation
     * @return {@link #getLiteral()}
     */
    public Code getCode(StaticEnvironment compilationEnv)
        throws SchemeException
    {
        return getConst().getLiteral();
    }

    /**
     * Compiles as normal code.
     * <p>
     * @return <code>getCode(compilationEnv)</code>
     */
    public Syntax getSyntax(StaticEnvironment compilationEnv)
        throws SchemeException
    {
        return ProcedureCall.create(
            getCode(
                compilationEnv
            )
        );
    }
}
