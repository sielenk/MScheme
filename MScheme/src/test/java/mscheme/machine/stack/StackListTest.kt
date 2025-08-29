/*
 * Created on 02.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.machine.stack;

import mscheme.exceptions.SchemeRuntimeError;
import mscheme.machine.StackFrame;
import mscheme.machine.stack.Stack.Mark;

/**
 * @author sielenk
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class StackListTest extends StackTest {

  public StackListTest(String name) {
    super(name);
  }

  private Stack _stack;

  protected IStack createStack() {
    return _stack = new Stack();
  }


  public final void testMark() throws SchemeRuntimeError {
    final int SIZE1 = 5;
    final int SIZE2 = 3;
    final int SIZE = SIZE1 + SIZE2;

    assertTrue(SIZE1 >= 2);
    assertTrue(SIZE2 >= 2);

    StackFrame[] buffer = new StackFrame[SIZE];

    Mark mark = null;

    for (int i = 0; i < SIZE; ++i) {
      if (i == SIZE1) {
        mark = _stack.createMark();
        assertNotNull(mark);
      }

      _stack.push(buffer[i] = createFrame());
    }

    // a b c d e | f g h

    Stack.Slice slice = _stack.cutSlice(mark);
    assertNotNull(slice);

    // a b c d e

    try {
      _stack.cutSlice(mark);
      fail();
    } catch (Throwable t) {
    }

    assertSame(buffer[SIZE1 - 1], _stack.pop());

    // a b c d

    _stack.reinstate(slice);

    // a b c d | f g h

    assertSame(buffer[SIZE - 1], _stack.pop());

    // a b c d | f g

    _stack.reinstate(slice);

    // a b c d | f g | f g h

    assertSame(buffer[SIZE - 1], _stack.pop());

    // a b c d | f g | f g

    for (int i = 1; i < SIZE2; ++i) {
      _stack.pop();
    }

    // a b c d | f g

    assertSame(buffer[SIZE - 2], _stack.pop());

    // a b c d | f

    for (int i = 2; i < SIZE2; ++i) {
      _stack.pop();
    }

    // a b c d

    assertSame(buffer[SIZE1 - 2], _stack.pop());

    // a b c

    for (int i = 2; i < SIZE1; ++i) {
      _stack.pop();
    }

    // -

    assertTrue(_stack.isEmpty());
  }
}
