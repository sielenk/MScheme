/*
 * The translation function for Scheme's function call. Copyright (C) 2001
 * Marvin H. Sielenkemper
 *
 * This file is part of MScheme.
 *
 * MScheme is free software; you can redistribute it and/or modify it under the
 * terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 2 of the License, or (at your option) any later
 * version.
 *
 * MScheme is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
 * A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * MScheme; see the file COPYING. If not, write to the Free Software Foundation,
 * Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

package mscheme.syntax;

import mscheme.code.Application;
import mscheme.compiler.Compiler;
import mscheme.environment.StaticEnvironment;
import mscheme.exceptions.SchemeException;
import mscheme.values.IList;

public final class ProcedureCall
    implements ITranslator {

  public final static String CVS_ID = "$Id$";

  private final Object _head;

  private ProcedureCall(Object head) {
    _head = head;
  }

  public static ProcedureCall create(Object head) {
    return new ProcedureCall(head);
  }

  public Object translate(StaticEnvironment compilationEnv, IList arguments)
      throws SchemeException, InterruptedException {
    compilationEnv.setStateClosed();

    Object[] compiledList = arguments.getCompiledArray(compilationEnv, 1);

    compiledList[0] = new Compiler(compilationEnv).getForceable(_head);

    return Application.create(compiledList);
  }
}