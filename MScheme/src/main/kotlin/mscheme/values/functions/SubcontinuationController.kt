/*
 * Created on 03.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.values.functions

import mscheme.exceptions.SchemeException
import mscheme.machine.Registers
import mscheme.machine.stack.Stack
import mscheme.values.ListFactory
import mscheme.values.ValueTraits

class SubcontinuationController internal constructor(
    state: Registers
) : UnaryFunction() {
    private val _mark: Stack.Mark =
        state.stack.createMark()

    override fun checkedCall(state: Registers, fst: Any?): Any? =
        ValueTraits.apply(
            state,
            fst,
            ListFactory.create(
                Subcontinuation(state.stack.cutSlice(_mark))
            )
        )
}
