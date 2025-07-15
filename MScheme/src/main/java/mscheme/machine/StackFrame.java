/*
 * Created on 03.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.machine;

import mscheme.environment.DynamicEnvironment;

/**
 * @author sielenk
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class StackFrame {

  public final static String CVS_ID
      = "$Id$";

  final DynamicEnvironment environment;
  final IContinuation continuation;

  public StackFrame(DynamicEnvironment e, IContinuation k) {
    environment = e;
    continuation = k;
  }
}
