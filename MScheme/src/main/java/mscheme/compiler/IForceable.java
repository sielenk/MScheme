/*
 * Created on 03.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.compiler;

import mscheme.exceptions.CompileError;

/**
 * @author sielenk
 */
public interface IForceable {

  /**
   * The CVS id of the file containing this class.
   */
  String CVS_ID
      = "$Id$";


  Object force()
      throws CompileError;
}
