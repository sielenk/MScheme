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
package de.masitec.mscheme.syntax


object TranslatorFactory {
    val beginToken: ITranslator
        get() = Begin.INSTANCE_BEGIN

    val andToken: ITranslator
        get() = Begin.INSTANCE_AND

    val orToken: ITranslator
        get() = Begin.INSTANCE_OR

    val setToken: ITranslator
        get() = Set

    val defineToken: ITranslator
        get() = Define

    val defineSyntaxToken: ITranslator
        get() = DefineSyntax

    val lambdaToken: ITranslator
        get() = Lambda

    val letToken: ITranslator
        get() = Let

    val letStarToken: ITranslator
        get() = LetStar

    val letrecToken: ITranslator
        get() = Letrec

    val ifToken: ITranslator
        get() = If

    val quoteToken: ITranslator
        get() = Quote
}
