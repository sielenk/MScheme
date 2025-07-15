/*
 * Created on 03.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.values.functions;

import mscheme.exceptions.SchemeException;
import mscheme.machine.Registers;
import mscheme.machine.stack.Stack.Slice;


public class Subcontinuation
    extends UnaryFunction {

  private final Slice _slice;

  public Subcontinuation(Slice slice) {
    _slice = slice;
  }


  protected Object checkedCall(
      Registers state,
      Object argument)
      throws SchemeException {
    state.getStack().reinstate(_slice);
    return argument;
  }
}
