/*
 * Created on 02.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.machine;

import mscheme.exceptions.RuntimeError;

/**
 * @author sielenk
 */
class StackList implements Stack
{
	private final Entry _root;
	private final Mark  _call_cc_mark;
	private Entry       _head;

	StackList()
	{
		_root         = new Entry();
		_head         = new Entry(_root);
		_call_cc_mark = _head._mark;
	}

	public boolean isEmpty()
	{
		return getStack().isEmpty();
	}

	public StackFrame pop()
	{
		_head = getTosEntry();
		return _head._stack.pop();
	}

	public void push(StackFrame f)
	{
		_head._stack.push(f);
	}

	private Entry getTosEntry()
	{
		Entry e = _head;
		
		while (e._stack.isEmpty() && (e._next != _root))
		{
			e = e._next;
		}

		return e;
	}

	private Stack getStack()
	{
		return getTosEntry()._stack;
	}


	public static class Mark
	{
		public Slice cutSlice(StackList l) throws RuntimeError
		{
			final Entry leaf = l._head;
			      Entry root = leaf;

			while ((root._mark != this)  && (root._next != null))
			{
				root = root._next;
			}

			if (root._next != null)
			{
				l._head = root._next;
				root._next= null;
				return new Slice(root, leaf);
			}
			else
			{
				throw new RuntimeError(null, "");
			}
		}
	}

    public static class Slice
    {
		private final Entry _root;
    	private final Entry _leaf;

		Slice(Entry root, Entry leaf)
		{
			_root = root;
			_leaf = leaf;
		}
    }

	private static class Entry
	{
		public final static String CVS_ID
		   = "$Id$";

		Entry            _next;
        final Mark       _mark;
		final StackPlain _stack;

		Entry()
		{
			_next  = null;
			_mark  = null;
			_stack = new StackPlain();
		}

		Entry(Entry next)
		{
			_next  = next;
			_mark  = new Mark();
			_stack = new StackPlain();
		}

		Entry(Entry leaf, Entry base)
		{
			_next  = (leaf._next == null)
			         ? base
			         : new Entry(leaf._next, base);
			_mark  = leaf._mark;
			_stack = leaf._stack.getCopy();
		}
	}

	synchronized
	public Continuation getCurrentContinuation()
	throws RuntimeError
	{
		return new Continuation(this, _call_cc_mark);
	}

	public Mark createMark()
	{
		_head = new Entry(_head);
		return _head._mark;
	}

	public void reinstate(Slice slice)
	{
		_head = new Entry(slice._leaf, _head);
	}
}
