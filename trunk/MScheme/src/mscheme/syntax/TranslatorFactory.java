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



public abstract class TranslatorFactory
{
    public final static String CVS_ID
        = "$Id$";


    public static ITranslator getBeginToken()
    {
        return Begin.INSTANCE_BEGIN;
    }

    public static ITranslator getAndToken()
    {
        return Begin.INSTANCE_AND;
    }

    public static ITranslator getOrToken()
    {
        return Begin.INSTANCE_OR;
    }

    public static ITranslator getSetToken()
    {
        return Set.INSTANCE;
    }

    public static ITranslator getDefineToken()
    {
        return Define.INSTANCE;
    }

    public static ITranslator getDefineSyntaxToken()
    {
        return DefineSyntax.INSTANCE;
    }

    public static ITranslator getLambdaToken()
    {
        return Lambda.INSTANCE;
    }

    public static ITranslator getLetToken()
    {
        return Let.INSTANCE;
    }

    public static ITranslator getLetStarToken()
    {
        return LetStar.INSTANCE;
    }

    public static ITranslator getLetrecToken()
    {
        return Letrec.INSTANCE;
    }

    public static ITranslator getIfToken()
    {
        return If.INSTANCE;
    }

    public static ITranslator getQuoteToken()
    {
        return Quote.INSTANCE;
    }
}
