package MScheme.code;

import MScheme.Value;
import MScheme.Code;

import MScheme.machine.Registers;
import MScheme.machine.Continuation;


final public class Selection
    extends Code
{
    public final static String id
        = "$Id$";

    final private Code _test;
    final private Code _onTrue;
    final private Code _onFalse;

    public Selection(
        Code test,
        Code onTrue,
        Code onFalse
    )
    {
        _test    = test;
        _onTrue  = onTrue;
        _onFalse = onFalse;
    }

    public Code executionStep(Registers registers)
    {
        new Continuation(registers) {
            protected Code execute(
                Registers regs,
                Value     evaluatedTest
            ) {
                return evaluatedTest.isTrue() ? _onTrue : _onFalse; 
            }
        };

        return _test;
    }
}
