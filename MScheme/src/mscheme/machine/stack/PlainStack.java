/*
 * Created on 02.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.machine.stack;

import mscheme.machine.StackFrame;


/**
 * @author sielenk
 */
class PlainStack implements IStack
{
	public final static String CVS_ID
	    = "$Id$";

	private final static int INITIAL_STACK_SIZE = 0;

	private StackFrame _stack[];
	private int        _sp;


	public PlainStack(PlainStack other)
	{
		_sp    = other._sp;
		_stack = new StackFrame[_sp];

		System.arraycopy(other._stack, 0, _stack, 0, _sp);
	}

	public PlainStack()
	{
		_sp    = 0;
		_stack = new StackFrame[INITIAL_STACK_SIZE];
	}

	public PlainStack getCopy()
	{
		return new PlainStack(this);
	}

	public boolean isEmpty()
	{
		return _sp <= 0;
	}

	public StackFrame pop()
	{
		assertFull();
		return _stack[--_sp];
	}

	public void push(StackFrame frame)
	{
        if (_sp >= _stack.length)
            enlarge();

		_stack[_sp++] = frame;
	}


	private void assertFull()
	{
		if (isEmpty())
		{
			throw new ArrayIndexOutOfBoundsException();
		}
	}

	private void enlarge()
	{
		final int          length   = _stack.length;
		final StackFrame[] oldStack = _stack;
		final StackFrame[] newStack = new StackFrame[length * 2 + 1];

		System.arraycopy(oldStack, 0, newStack,0, length);

		_stack = newStack;
	}
}