/*
 * Created on 03.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.machine;

import mscheme.exceptions.SchemeException;
import mscheme.values.ListFactory;
import mscheme.values.ValueTraits;
import mscheme.values.functions.UnaryFunction;


public class Controller
        extends UnaryFunction
{
    public final static String CVS_ID = "$Id$";

    private final StackList.Mark _mark;

    Controller(StackList.Mark mark)
    {
        _mark = mark;
    }

    protected Object checkedCall(Registers state, Object argument)
            throws SchemeException, InterruptedException
    {
        return ValueTraits.apply(state, argument, ListFactory
                .create(new Subcontinuation(_mark.cutSlice(state.getStack()))));
    }
}