/*
 * Created on 02.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.machine;

import mscheme.exceptions.RuntimeError;
import mscheme.exceptions.SchemeException;
import mscheme.machine.StackList.Slice;
import mscheme.machine.StackList.Mark;
import mscheme.values.functions.UnaryFunction;

/**
 * @author sielenk
 *
 */
public class Continuation
	extends UnaryFunction
{
	public final static String CVS_ID
		= "$Id$";


	private final Slice _slice;
	private final Mark  _mark;

	public Continuation(StackList stack, Mark mark)
	throws RuntimeError
	{
		_slice = mark.cutSlice(stack);
		_mark  = mark;

		stack.reinstate(_slice);
	}

	protected Object checkedCall(
		Registers state,
		Object    argument)
	throws SchemeException
	{
		_mark.cutSlice(state.getStack());
		state.getStack().reinstate(_slice);
		return argument;
	}
}
