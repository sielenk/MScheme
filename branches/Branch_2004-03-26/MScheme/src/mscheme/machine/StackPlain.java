/*
 * Created on 02.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.machine;

/**
 * @author sielenk
 */
class StackPlain implements Stack
{
	public final static String id
	    = "$Id$";

	private static int INITIAL_STACK_SIZE = 0;

	private StackFrame _stack[];
	private int        _sp;


	public StackPlain(StackPlain other)
	{
		_sp    = other._sp;
		_stack = new StackFrame[_sp];

		System.arraycopy(other._stack, 0, _stack, 0, _sp);
	}

	public StackPlain()
	{
		_sp    = 0;
		_stack = new StackFrame[INITIAL_STACK_SIZE];
	}

	public StackPlain getCopy()
	{
		return new StackPlain(this);
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
		if (_sp == _stack.length)
		{
			enlarge();
		}

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
		final int          oldLength   = _stack.length;
		final int          newLength   = oldLength * 2 + 1;
		final StackFrame[] oldStack = _stack;
		final StackFrame[] newStack = new StackFrame[newLength];

		System.arraycopy(oldStack, 0, newStack,0, oldLength);

		_stack = newStack;
	}
}