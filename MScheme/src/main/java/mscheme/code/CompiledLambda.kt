/*
 * The implementation of scheme's 'lambda'. Copyright (C) 2001 Marvin H.
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

import java.io.IOException;
import java.io.Writer;
import mscheme.compiler.Compiler;
import mscheme.compiler.IForceable;
import mscheme.environment.DynamicEnvironment;
import mscheme.environment.StaticEnvironment;
import mscheme.exceptions.CompileError;
import mscheme.exceptions.ListExpected;
import mscheme.exceptions.PairExpected;
import mscheme.exceptions.SchemeException;
import mscheme.machine.Registers;
import mscheme.util.Arity;
import mscheme.values.IList;
import mscheme.values.functions.CheckedFunction;
import org.jetbrains.annotations.NotNull;

public final class CompiledLambda
    implements IForceable, IReduceable {

  private final Arity _arity;

  private final int _frameSize;

  private Object _compiledBody;

  private CompiledLambda(Arity arity, int frameSize, Object compiledBody) {
    _arity = arity;
    _frameSize = frameSize;
    _compiledBody = compiledBody;
  }

  public static CompiledLambda create(Arity arity, int frameSize,
      Object compiledBody) {
    return new CompiledLambda(arity, frameSize, compiledBody);
  }

  public static CompiledLambda create(Arity arity, IList body,
      StaticEnvironment env)
      throws SchemeException, InterruptedException {
    Object compiledBody = Sequence.create(body.getCompiledArray(env));

    return create(arity, env.getSize(), compiledBody);
  }

  final class Closure
      extends CheckedFunction {

    private final DynamicEnvironment _enclosingEnvironment;

    public Closure(DynamicEnvironment enclosingEnvironment) {
      _enclosingEnvironment = enclosingEnvironment;
    }

    public void write(Writer destination)
        throws IOException {
      destination.write("#[closure]");
    }

    public Arity getArity() {
      return _arity;
    }

    protected Object checkedCall(Registers state, IList arguments)
        throws ListExpected, PairExpected {
      DynamicEnvironment newEnvironment = _enclosingEnvironment
          .createChild(_arity, _frameSize, arguments);

      state.setEnvironment(newEnvironment);

      return _compiledBody;
    }
  }

  public Object force()
      throws CompileError {
    _compiledBody = Compiler.force(_compiledBody);
    return this;
  }

  public String toString() {
    return "lambda:<" + _compiledBody + '>';
  }

  public Object reduce(@NotNull Registers state) {
    return new Closure(state.getEnvironment());
  }
}