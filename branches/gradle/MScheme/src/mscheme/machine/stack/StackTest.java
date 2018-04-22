/*
 * Created on 02.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.machine.stack;

import mscheme.machine.StackFrame;
import junit.framework.TestCase;

/**
 * @author sielenk
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public abstract class StackTest extends TestCase
{
	public final static String CVS_ID
	   = "$Id$";

	private IStack _stack;

	protected abstract IStack createStack();


	public StackTest(String name)
	{
		super(name);
	}

	protected void setUp() throws Exception
	{
		super.setUp();
		_stack = createStack();
	}

	protected void tearDown() throws Exception
	{
		super.tearDown();
		_stack = null;
	}


	protected static StackFrame createFrame()
	{
		return new StackFrame(null,null);
	}

	public void testCreateSomething()
	{
		assertNotSame(createFrame(), createFrame());
		assertNotSame(createFrame(), createFrame());
	}

	public void testStack()
	{
		assertNotSame(null, _stack);
	}

	public void testIsEmpty()
	{
		assertTrue(_stack.isEmpty());
	}

	public void testPush()
	{
		_stack.push(null);

		assertFalse(_stack.isEmpty());
	}

	public void testPop1()
	{
		try
		{
			_stack.pop();
			fail();		
		}
		catch (Throwable t)
		{ }
	}

	public void testPop2()
	{
		StackFrame frame1 = createFrame();
		StackFrame frame2 = createFrame();
		StackFrame frame3 = createFrame();

		_stack.push(frame1);
		_stack.push(frame2);
		_stack.push(frame3);

		assertFalse(_stack.isEmpty());
		assertSame(frame3,  _stack.pop());

		assertFalse(_stack.isEmpty());
		assertSame(frame2,  _stack.pop());

		assertFalse(_stack.isEmpty());
		assertSame(frame1,  _stack.pop());
		
		assertTrue(_stack.isEmpty());
	}
}
