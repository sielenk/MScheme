/* The translation function for Scheme's 'define-syntax'.
   Copyright (C) 2001  Marvin H. Sielenkemper

This file is part of MScheme.

MScheme is free software; you can redistribute it and/or modify 
it under the terms of the GNU General Public License as published by 
the Free Software Foundation; either version 2 of the License, 
or (at your option) any later version. 

MScheme is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details. 

You should have received a copy of the GNU General Public License
along with MScheme; see the file COPYING. If not, write to 
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA  02111-1307, USA. */

package mscheme.syntax;

import mscheme.code.Application;
import mscheme.compiler.Compiler;
import mscheme.environment.Environment;
import mscheme.environment.StaticEnvironment;
import mscheme.exceptions.SchemeException;
import mscheme.machine.Machine;
import mscheme.util.Arity;
import mscheme.values.IList;
import mscheme.values.ListFactory;
import mscheme.values.IPair;
import mscheme.values.Symbol;
import mscheme.values.ValueTraits;
import mscheme.values.functions.ApplyFunction;


final class Macro
    implements ITranslator
{
    public final static String CVS_ID
        = "$Id$";


    final static Machine MACHINE = 
        new Machine(
            Environment.getSchemeReportEnvironment()
        );

    private final static Object APPLY
        = ApplyFunction.INSTANCE;

    private final Object            _transformer;
    private final StaticEnvironment _definitionEnv;

    Macro(Object transformer, StaticEnvironment definitionEnv)
    {
        _transformer   = transformer;
        _definitionEnv = definitionEnv;
    }

    public Object translate(
        StaticEnvironment usageEnv,
        IList              arguments
    ) throws SchemeException, InterruptedException
    {
        // (apply tranformer def_env use_env args)

        IPair result = (IPair)MACHINE.execute(
            Application.create(
                new Object[]
                {
                    APPLY,
                    _transformer,
                    _definitionEnv,
                          usageEnv,
                         arguments
                }
            )
        );

        return new Compiler(
            ValueTraits.toStaticEnvironment(result.getFirst())
        ).getForceable(
        	result.getSecond());
    }
}

final class DefineSyntax
    extends CheckedTranslator
{
    public final static String CVS_ID
        = "$Id$";


    final static ITranslator INSTANCE = new DefineSyntax();

    private DefineSyntax()
    {
        super(Arity.exactly(2));
    }

    protected Object checkedTranslate(
        StaticEnvironment compilationEnv,
        IList              arguments
    ) throws SchemeException, InterruptedException
    {
        Symbol symbol = ValueTraits.toSymbol(arguments.getHead());
        Object value  = arguments.getTail().getHead();

        Macro macro = new Macro(
           	Macro.MACHINE.evaluate(value),
            compilationEnv);

        compilationEnv.defineSyntax(symbol, macro);
        Macro
            .MACHINE
            .getEnvironment()
            .getStatic()
            .defineSyntax(symbol, macro);

        return ListFactory.create();
    }
}
