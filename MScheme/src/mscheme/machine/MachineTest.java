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
	extends TestCase
{
	public final static String id
		= "$Id$";


	private final static Object _o1 = new Object();
	private final static Object _o2 = new Object();
	private final static Object _o3 = new Object();

	private Environment _environment;
	private Reference   _key;
	private Machine     _machine;

	/**
	 * Constructor for MachineTest.
	 * @param name
	 */
	public MachineTest(String name)
	{
		super(name);
	}

	/*
	 * @see TestCase#setUp()
	 */
	protected void setUp() throws Exception
	{
		super.setUp();

		_environment = Environment.getNullEnvironment();
		_key         = _environment.define(ValueTraits.createUniqueSymbol(), _o1);
		_machine     = new Machine(_environment);
	}

	/*
	 * @see TestCase#tearDown()
	 */
	protected void tearDown() throws Exception
	{
		super.tearDown();

		_machine     = null;
		_key         = null;
		_environment = null;
	}
	
	final public void testObjects() throws SchemeException
	{
		assertSame(null, null);
		assertSame(_o1, _o1);
		assertSame(_o2, _o2);
		assertSame(_o3, _o3);

		assertNotNull(_o1);
		assertNotNull(_o2);
		assertNotNull(_o3);

		assertNotSame(_o1, _o2);
		assertNotSame(_o1, _o3);
		assertNotSame(_o2, _o3);

		assertSame(_o1, _machine.execute(_o1));
		assertSame(_o2, _machine.execute(_o2));
		assertSame(_o3, _machine.execute(_o3));
	}

	final private Object execSelection(
		Object test,
		Object onTrue,
		Object onFalse) throws Exception
	{
		return _machine.execute(
		    (Object)new Selection(test, onTrue, onFalse));
	}

	final public void testSelection() throws Exception
	{
		assertSame(_o1, execSelection(Boolean.TRUE,  _o1, _o2));
		assertSame(_o2, execSelection(Boolean.FALSE, _o1, _o2));

		assertSame(_o1, execSelection(_o1, _o1, _o2));
		assertSame(_o1, execSelection(_o2, _o1, _o2));
		assertSame(_o1, execSelection(_o3, _o1, _o2));
	}

	final private Object execAssign(
		Object    value) throws Exception
	{
		_machine.execute(
			(Object)new Assignment(_key, value));

		return _environment.lookup(_key);
	}

	final public void testAssign() throws Exception
	{
		assertSame(_o2, execAssign(_o2));
		assertSame(_o3, execAssign(_o3));
	}

	final public void testSequence() throws Exception
	{
		Object[]  sequence = { new Assignment(_key, _o3), _o2 };
		
		assertSame(_o1, _environment.lookup(_key));
		assertSame(_o2, _machine.execute((Object)Sequence.create(sequence)));
		assertSame(_o3, _environment.lookup(_key));
	}

	final public void testConj() throws Exception
	{
		Object    assignment = new Assignment(_key, _o2);
		Object[]  sequence1  = { Boolean.FALSE, assignment };
		Object[]  sequence2  = { Boolean.TRUE,  assignment };

		assertSame(_o1, _environment.lookup(_key));
		_machine.execute((Object)Sequence.createConj(sequence1));
		assertSame(_o1, _environment.lookup(_key));
		_machine.execute((Object)Sequence.createConj(sequence2));
		assertSame(_o2, _environment.lookup(_key));
	}

	final public void testDisj() throws Exception
	{
		Object    assignment = new Assignment(_key, _o2);
		Object[]  sequence1  = { Boolean.TRUE,  assignment };
		Object[]  sequence2  = { Boolean.FALSE, assignment };

		assertSame(_o1, _environment.lookup(_key));
		_machine.execute((Object)Sequence.createDisj(sequence1));
		assertSame(_o1, _environment.lookup(_key));
		_machine.execute((Object)Sequence.createDisj(sequence2));
		assertSame(_o2, _environment.lookup(_key));
	}

	final public void testApplication() throws Exception
	{
		Object[] sequence = { AppendFunction.INSTANCE };

		assertTrue(
			ValueTraits.isEmptyList(
			    _machine.execute((Object)Application.create(sequence))));
	}
}
