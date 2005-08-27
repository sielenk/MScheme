/*
 * Created on 02.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.values.functions;

import mscheme.exceptions.RuntimeError;
import mscheme.exceptions.SchemeException;
import mscheme.machine.Registers;
import mscheme.machine.stack.Stack.Slice;

/**
 * @author sielenk
 */
public class Continuation
	extends UnaryFunction
{
	public final static String CVS_ID
		= "$Id$";


	private final Slice _slice;

	public Continuation(Registers state) throws RuntimeError
    {
        _slice = state.getStack().getContinuation();
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
