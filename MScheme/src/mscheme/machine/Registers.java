/*
 * Created on 02.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.machine;

import mscheme.environment.DynamicEnvironment;
import mscheme.environment.Reference;
import mscheme.exceptions.RuntimeError;

/**
 * @author sielenk
 */
public class Registers
{
	/** The CVS id of the file containing this class. */
	public final static String id
		= "$Id$";


	private StackList          _stack;
	private DynamicEnvironment _environment;


	Registers(DynamicEnvironment environment)
	{
		_stack       = new StackList();
		_environment = environment;
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

	public void push(Invokeable k)
	{
		_stack.push(new StackFrame(_environment, k));
	}

	public Object assign(Reference key, Object value) {
		return _environment.assign(key, value);
	}


	public Controller getController()
	{
		return new Controller(_stack.createMark());
	}

	public Continuation getCurrentContinuation() throws RuntimeError
	{
		return _stack.getCurrentContinuation();		
	}
}
