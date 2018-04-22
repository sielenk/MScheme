/*
 * Created on 02.01.2004
 *
 */
package mscheme.machine.stack;


/**
 * @author sielenk
 */
public class StackPlainTest extends StackTest
{
	public StackPlainTest(String name) {
		super(name);
	}

	private PlainStack _stack;

	protected IStack createStack()
	{
		return _stack = new PlainStack();
	}

	public void testCopy()
	{
		final int SIZE = 50;

		for (int i = 0; i < SIZE; ++i)
		{ 
			_stack.push(createFrame());
		}

		IStack otherStack = _stack.getCopy();

		for (int j = 0; j < SIZE; ++j)
		{ 
			assertFalse(_stack.isEmpty());
			assertFalse(otherStack.isEmpty());

			assertSame(_stack.pop(),  otherStack.pop() );
		}

		assertTrue(_stack.isEmpty());
		assertTrue(otherStack.isEmpty());
	}
}
