/* The translation function for Scheme's function call.
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

import MScheme.Syntax;
import MScheme.Code;

import MScheme.values.List;

import MScheme.util.Arity;

import MScheme.code.CodeList;
import MScheme.code.Application;

import MScheme.environment.StaticEnvironment;

import MScheme.exceptions.SchemeException;


public final class ProcedureCall
    implements Syntax
{
    public final static String id
        = "$Id$";


    private final Code _head;

    private ProcedureCall(Code head)
    {
        _head = head;
    }

    public static ProcedureCall create(Code head)
    {
        return new ProcedureCall(head);
    }

    public Code translate(
        StaticEnvironment compilationEnv,
        List              arguments
    ) throws SchemeException
    {
        return Application.create(
            CodeList.prepend(
                _head,
                arguments.getCodeList(compilationEnv)
            )
        );
    }
}
