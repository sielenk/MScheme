/* The registers of the 'virtual scheme machine'.
   Copyright (C) 2004  Marvin H. Sielenkemper

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

package mscheme.machine;

import mscheme.code.Reduceable;
import mscheme.environment.DynamicEnvironment;
import mscheme.environment.Reference;
import mscheme.exceptions.RuntimeError;
import mscheme.exceptions.SchemeException;
import mscheme.values.ListFactory;
import mscheme.values.ValueTraits;
import mscheme.values.functions.UnaryFunction;

/**
 * @author sielenk
 */
public class Registers
{
    /** The CVS id of the file containing this class. */
    public final static String id =
        "$Id$";

    private StackList _stack;
    private DynamicEnvironment _environment;
    private Object _expression;

    Registers(DynamicEnvironment environment)
    {
    }

    /**
     * @param environment
     * @param expression
     */
    public Registers(DynamicEnvironment environment, Object expression)
    {
        _stack = new StackList();
        _environment = environment;
        _expression = expression;
    }

    StackList getStack()
    {
        return _stack;
    }

    public void setEnvironment(DynamicEnvironment environment)
    {
        _environment = environment;
    }

    public DynamicEnvironment getEnvironment()
    {
        return _environment;
    }

    public void setExpression(Object expression)
    {
        _expression = expression;
    }

    public Object getExpression()
    {
        return _expression;
    }

    public void push(Invokeable k)
    {
        _stack.push(new StackFrame(_environment, k));
    }

    public Object assign(Reference key, Object value)
    {
        return _environment.assign(key, value);
    }

    public UnaryFunction getController()
    {
        return new Controller(_stack.createMark());
    }

    public Continuation getCurrentContinuation() throws RuntimeError
    {
        return _stack.getCurrentContinuation();
    }

    /**
     * @return
     */
    boolean isEvaluated()
    {
        return !(_expression instanceof Reduceable) && _stack.isEmpty();
    }

	/**
	 * @return <code>true</code> on reduction, <code>false</code> on invocation
	 */
    boolean step() throws SchemeException
    {
        if (_expression instanceof Reduceable)
        {
            _expression = ((Reduceable)_expression).reduce(this);

            return true;
        }
        else
        {
            final StackFrame frame = _stack.pop();
            _environment = frame.environment;
            _expression = frame.invokeable.invoke(this, _expression);

            return false;
        }
    }
}

final class Controller extends UnaryFunction
{
    private final StackList.Mark _mark;

    Controller(StackList.Mark mark)
    {
        _mark = mark;
    }

    protected Object checkedCall(final Registers state, Object argument)
        throws SchemeException
    {
        return ValueTraits.apply(
            state,
            argument,
            ListFactory.create(
                new Subcontinuation(
                	_mark.cutSlice(
                		state.getStack()))));
    }
};

final class Subcontinuation extends UnaryFunction
{
    private final StackList.Slice _slice;

    Subcontinuation(StackList.Slice slice)
    {
        _slice = slice;
    }

    protected Object checkedCall(Registers state, Object argument)
        throws SchemeException
    {
        state.getStack().reinstate(_slice);
        return argument;
    }
}