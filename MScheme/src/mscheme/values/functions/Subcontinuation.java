/*
 * Created on 03.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.values.functions;

import mscheme.exceptions.RuntimeError;
import mscheme.exceptions.SchemeException;
import mscheme.machine.Registers;
import mscheme.machine.stack.Stack.Slice;


public class Subcontinuation
	extends UnaryFunction
{
	public final static String CVS_ID
		= "$Id$";


	private final Slice _slice;

    public Subcontinuation(Registers state) throws RuntimeError
    {
        _slice = state.getStack().getContinuation();
    }

	Subcontinuation(Slice slice)
	{
		_slice = slice;
	}


	protected Object checkedCall(
		Registers state,
		Object    argument)
	throws SchemeException
	{
		state.getStack().reinstate(_slice);
		return argument;
	}
}
