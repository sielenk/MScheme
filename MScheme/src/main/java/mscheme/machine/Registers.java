/*
 * Created on 02.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.machine;

import mscheme.environment.DynamicEnvironment;
import mscheme.environment.Reference;
import mscheme.machine.stack.Stack;

/**
 * @author sielenk
 */
public class Registers
{
	/** The CVS id of the file containing this class. */
	public final static String CVS_ID
		= "$Id$";


	private final Stack        _stack;
	private DynamicEnvironment _environment;


	Registers(DynamicEnvironment environment)
	{
		_stack       = new Stack();
		_environment = environment;
	}

	public Stack getStack()
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

	public void push(IContinuation k)
	{
		_stack.push(new StackFrame(_environment, k));
	}

	public Object assign(Reference key, Object value)
	{
		return _environment.assign(key, value);
	}
}
