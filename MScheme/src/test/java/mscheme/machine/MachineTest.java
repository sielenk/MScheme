/*
 * Created on 02.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.machine;

import junit.framework.TestCase;
import mscheme.code.Application;
import mscheme.code.Assignment;
import mscheme.code.Selection;
import mscheme.code.Sequence;
import mscheme.environment.Environment;
import mscheme.environment.Reference;
import mscheme.exceptions.SchemeException;
import mscheme.values.ValueTraits;
import mscheme.values.functions.AppendFunction;

/**
 * @author sielenk
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class MachineTest
    extends TestCase {

  private final static Object O1 = new Object();
  private final static Object O2 = new Object();
  private final static Object O3 = new Object();

  private Environment _environment;
  private Reference _key;
  private Machine _machine;

  /**
   * Constructor for MachineTest.
   */
  public MachineTest(String name) {
    super(name);
  }

  /*
   * @see TestCase#setUp()
   */
  protected void setUp() throws Exception {
    super.setUp();

    _environment = Environment.getNullEnvironment();
    _key = _environment.define(ValueTraits.createUniqueSymbol(), O1);
    _machine = new Machine(_environment);
  }

  /*
   * @see TestCase#tearDown()
   */
  protected void tearDown() throws Exception {
    super.tearDown();

    _machine = null;
    _key = null;
    _environment = null;
  }

  final public void testObjects() throws SchemeException, InterruptedException {
    assertSame(null, null);
    assertSame(O1, O1);
    assertSame(O2, O2);
    assertSame(O3, O3);

    assertNotNull(O1);
    assertNotNull(O2);
    assertNotNull(O3);

    assertNotSame(O1, O2);
    assertNotSame(O1, O3);
    assertNotSame(O2, O3);

    assertSame(O1, _machine.execute(O1));
    assertSame(O2, _machine.execute(O2));
    assertSame(O3, _machine.execute(O3));
  }

  private Object execSelection(
      Object test,
      Object onTrue,
      Object onFalse) throws Exception {
    return _machine.evaluate(
        Selection.create(test, onTrue, onFalse));
  }

  final public void testSelection() throws Exception {
    assertSame(O1, execSelection(Boolean.TRUE, O1, O2));
    assertSame(O2, execSelection(Boolean.FALSE, O1, O2));

    assertSame(O1, execSelection(O1, O1, O2));
    assertSame(O1, execSelection(O2, O1, O2));
    assertSame(O1, execSelection(O3, O1, O2));
  }

  private Object execAssign(
      Object value) throws Exception {
    _machine.execute(
        Assignment.create(_key, value));

    return _environment.lookup(_key);
  }

  final public void testAssign() throws Exception {
    assertSame(O2, execAssign(O2));
    assertSame(O3, execAssign(O3));
  }

  final public void testSequence() throws Exception {
    Object[] sequence = {Assignment.create(_key, O3), O2};

    assertSame(O1, _environment.lookup(_key));
    assertSame(O2, _machine.execute(Sequence.create(sequence)));
    assertSame(O3, _environment.lookup(_key));
  }

  final public void testConj() throws Exception {
    Object assignment = Assignment.create(_key, O2);
    Object[] sequence1 = {Boolean.FALSE, assignment};
    Object[] sequence2 = {Boolean.TRUE, assignment};

    assertSame(O1, _environment.lookup(_key));
    _machine.execute(Sequence.createConj(sequence1));
    assertSame(O1, _environment.lookup(_key));
    _machine.execute(Sequence.createConj(sequence2));
    assertSame(O2, _environment.lookup(_key));
  }

  final public void testDisj() throws Exception {
    Object assignment = Assignment.create(_key, O2);
    Object[] sequence1 = {Boolean.TRUE, assignment};
    Object[] sequence2 = {Boolean.FALSE, assignment};

    assertSame(O1, _environment.lookup(_key));
    _machine.execute(Sequence.createDisj(sequence1));
    assertSame(O1, _environment.lookup(_key));
    _machine.execute(Sequence.createDisj(sequence2));
    assertSame(O2, _environment.lookup(_key));
  }

  final public void testApplication() throws Exception {
    Object[] sequence = {AppendFunction.INSTANCE};

    assertTrue(
        ValueTraits.isEmpty(
            _machine.execute(Application.create(sequence))));
  }
}
