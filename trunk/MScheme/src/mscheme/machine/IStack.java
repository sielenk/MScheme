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
interface IStack {
	public abstract boolean isEmpty();
	public abstract StackFrame pop();
	public abstract void push(StackFrame frame);
}
