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

class Subcontinuation(private val _slice: Stack.Slice) : UnaryFunction() {
    @Throws(SchemeException::class)
    override fun checkedCall(state: Registers, fst: Any?): Any? {
        state.stack.reinstate(_slice)
        return fst
    }
}
