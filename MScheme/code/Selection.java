package MScheme.code;

import MScheme.Value;
import MScheme.Code;

import MScheme.machine.Registers;
import MScheme.machine.Continuation;


public final class Selection
    extends Code
{
    public final static String id
        = "$Id$";

    private final Code _test;
    private final Code _onTrue;
    private final Code _onFalse;

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
            public final static String id
                = "$Id$";

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
