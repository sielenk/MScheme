/* The translation function for Scheme's 'begin'.
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

import mscheme.Syntax;

import mscheme.code.Sequence;

import mscheme.environment.StaticEnvironment;

import mscheme.exceptions.SchemeException;

import mscheme.util.Arity;

import mscheme.values.List;


final class Begin
    extends    CheckedSyntax
    implements SequenceTags
{
    public final static String id
        = "$Id$";

    private final int _tag;

    final static Syntax INSTANCE_BEGIN = new Begin(TAG_BEGIN);
    final static Syntax INSTANCE_AND   = new Begin(TAG_AND  );
    final static Syntax INSTANCE_OR    = new Begin(TAG_OR   );

    private Begin(int tag)
    {
        super(Arity.atLeast((tag == TAG_BEGIN) ? 1 : 0));
        _tag = tag;
    }

    protected void preTranslate(StaticEnvironment compilationEnv)
    {
        if (_tag != TAG_BEGIN)
        {
            super.preTranslate(compilationEnv);
        }
    }

    protected Object checkedTranslate(
        StaticEnvironment compilationEnv,
        List              arguments
    ) throws SchemeException
    {
        return Sequence.create(
            _tag,
            arguments.getCompiledArray(compilationEnv)
        );
    }
}
