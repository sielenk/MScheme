/*
 * Created on 03.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.machine;

import mscheme.exceptions.SchemeException;
import mscheme.machine.StackList.Slice;
import mscheme.values.functions.UnaryFunction;


public class Subcontinuation
	extends UnaryFunction
{
	public final static String CVS_ID
		= "$Id$";


	private final Slice _slice;

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
