/* The translation function for Scheme's 'if'.
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

import MScheme.Value;
import MScheme.Code;
import MScheme.Syntax;

import MScheme.util.Arity;

import MScheme.code.Selection;

import MScheme.environment.StaticEnvironment;

import MScheme.values.Function;
import MScheme.values.ScmBoolean;
import MScheme.values.List;

import MScheme.exceptions.SchemeException;


final class If
    extends CheckedSyntax
{
    public final static String id
    = "$Id$";


    final static Syntax INSTANCE = new If();

    private If()
    {
        super(Arity.inRange(2, 3));
    }


    protected Code checkedTranslate(
        StaticEnvironment compilationEnv,
        List              arguments
    ) throws SchemeException
    {
        Value flag    = arguments.getHead();
        Value onTrue  = arguments.getTail().getHead();
        Value onFalse =
            arguments.getTail().getTail().isEmpty()
            ? ScmBoolean.createFalse()
            : arguments.getTail().getTail().getHead();

        return Selection.create(
                   flag.   getCode(compilationEnv),
                   onTrue. getCode(compilationEnv),
                   onFalse.getCode(compilationEnv)
               );
    }
}
