/* The factory class for the translators.
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


public abstract class SyntaxFactory
{
    public final static String id
        = "$Id$";


    public static Syntax getBeginToken()
    {
        return Begin.INSTANCE_BEGIN;
    }

    public static Syntax getAndToken()
    {
        return Begin.INSTANCE_AND;
    }

    public static Syntax getOrToken()
    {
        return Begin.INSTANCE_OR;
    }

    public static Syntax getSetToken()
    {
        return Set.INSTANCE;
    }

    public static Syntax getDefineToken()
    {
        return Define.INSTANCE;
    }

    public static Syntax getDefineSyntaxToken()
    {
        return DefineSyntax.INSTANCE;
    }

    public static Syntax getLambdaToken()
    {
        return Lambda.INSTANCE;
    }

    public static Syntax getLetToken()
    {
        return Let.INSTANCE;
    }

    public static Syntax getLetStarToken()
    {
        return LetStar.INSTANCE;
    }

    public static Syntax getLetrecToken()
    {
        return Letrec.INSTANCE;
    }

    public static Syntax getIfToken()
    {
        return If.INSTANCE;
    }

    public static Syntax getQuoteToken()
    {
        return Quote.INSTANCE;
    }
}
