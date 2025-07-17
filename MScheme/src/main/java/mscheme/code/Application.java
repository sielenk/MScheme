/*
 * The implementation of Scheme's function call. Copyright (C) 2001 Marvin H.
 * Sielenkemper
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

package mscheme.code;

import mscheme.compiler.IForceable;
import mscheme.exceptions.CompileError;
import mscheme.exceptions.SchemeException;
import mscheme.machine.IContinuation;
import mscheme.machine.Registers;
import mscheme.values.IList;
import mscheme.values.ListFactory;
import mscheme.values.ValueTraits;
import org.jetbrains.annotations.NotNull;

public final class Application
    implements IForceable, IReduceable {

  private final Object[] _application;

  private Application(Object[] application) {
    _application = application;
  }

  public static Application create(Object[] application) {
    return new Application(application);
  }

  public Object force()
      throws CompileError {
    CodeArray.force(_application);
    return this;
  }

  public String toString() {
    return "app:" + CodeArray.printTuple(_application);
  }

  public Object reduce(@NotNull Registers registers) {
    return prepareNext(registers, ListFactory.create(),
        _application.length - 1);
  }

  public static IContinuation createCall(final IList done) {
    return new IContinuation() {
      public Object invoke(Registers registers, Object value)
          throws SchemeException, InterruptedException {
        return ValueTraits.apply(registers, value, done);
      }
    };
  }

  private IContinuation createPush(final IList done, final int index) {
    return new IContinuation() {
      public Object invoke(Registers registers, Object value) {
        return prepareNext(registers, ListFactory.prepend(value, done),
            index - 1);
      }
    };
  }

  private Object prepareNext(Registers registers, final IList done,
      final int index) {
    registers.push((index == 0) ? createCall(done)
        : createPush(done, index));

    return _application[index];
  }
}