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

package MScheme.syntax;

import MScheme.Code;
import MScheme.Syntax;
import MScheme.Value;

import MScheme.code.Application;

import MScheme.environment.Environment;
import MScheme.environment.StaticEnvironment;

import MScheme.exceptions.SchemeException;

import MScheme.machine.Machine;

import MScheme.util.Arity;

import MScheme.values.Empty;
import MScheme.values.List;
import MScheme.values.Pair;
import MScheme.values.Symbol;

import MScheme.values.functions.ApplyFunction;


final class Macro
    implements Syntax
{
    public final static String id
        = "$Id$";


    final static Machine machine = 
        new Machine(
            Environment.getSchemeReportEnvironment()
        );

    private final static Code _apply
        = ApplyFunction.INSTANCE;

    private final Code              _transformer;
    private final StaticEnvironment _definitionEnv;

    Macro(Code transformer, StaticEnvironment definitionEnv)
    {
        _transformer   = transformer;
        _definitionEnv = definitionEnv;
    }

    public Code translate(
        StaticEnvironment usageEnv,
        List              arguments
    ) throws SchemeException
    {
        // (apply tranformer def_env use_env args)

        Pair result = machine.execute(
            Application.create(
                new Code[]
                {
                    _apply,
                    _transformer,
                    _definitionEnv,
                          usageEnv,
                         arguments
                }
            )
        ).toPair();

        return
            result
            .getSecond()
            .getCompiled(
                result
                .getFirst()
                .toStaticEnvironment()
            );
    }
}

final class DefineSyntax
    extends CheckedSyntax
{
    public final static String id
        = "$Id$";


    final static Syntax INSTANCE = new DefineSyntax();

    private DefineSyntax()
    {
        super(Arity.exactly(2));
    }

    protected Code checkedTranslate(
        StaticEnvironment compilationEnv,
        List              arguments
    ) throws SchemeException
    {
        Symbol symbol = arguments.getHead().toSymbol();
        Value  value  = arguments.getTail().getHead();

        Macro macro = new Macro(
            Macro.machine.evaluate(
                value
            ).toFunction(),
            compilationEnv
        );

        compilationEnv.defineSyntax(symbol, macro);
        Macro
            .machine
            .getEnvironment()
            .getStatic()
            .defineSyntax(symbol, macro);

        return Empty.create();
    }
}
