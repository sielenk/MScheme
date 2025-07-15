/*
 * Created on 03.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.values.functions;

import mscheme.exceptions.SchemeException;
import mscheme.machine.Registers;
import mscheme.machine.stack.Stack.Mark;
import mscheme.values.ListFactory;
import mscheme.values.ValueTraits;


public class SubcontinuationController
    extends UnaryFunction {

  public final static String CVS_ID = "$Id$";

  private final Mark _mark;

  SubcontinuationController(Registers state) {
    _mark = state.getStack().createMark();
  }

  protected Object checkedCall(Registers state, Object argument)
      throws SchemeException, InterruptedException {
    return ValueTraits.apply(
        state,
        argument,
        ListFactory.create(
            new Subcontinuation(
                state.getStack().cutSlice(_mark))));
  }
}