/*
 * Copyright (C) 2025  Marvin H. Sielenkemper
 *
 * This file is part of MScheme.
 *
 * MScheme is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License,
 * or (at your option) any later version.
 *
 * MScheme is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with MScheme; see the file COPYING. If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA  02111-1307, USA.
 */

package mscheme.values.functions;

import mscheme.exceptions.RuntimeArityError;
import mscheme.exceptions.SchemeRuntimeError;
import mscheme.exceptions.TypeError;
import mscheme.util.Arity;
import mscheme.values.IList;
import mscheme.values.ScmNumber;
import mscheme.values.ValueTraits;


final class Order {

  public final static int LT = -2;

  public final static int LE = -1;

  public final static int EQ = 0;

  public final static int GE = 1;

  public final static int GT = 2;

  public static boolean check(IList arguments, int mode)
      throws SchemeRuntimeError, TypeError {
    final Arity arity = Arity.atLeast(2);
    int len = arguments.getLength();

    if (!arity.isValid(len)) {
      throw new RuntimeArityError(arguments, arity);
    }

    ScmNumber curr = ValueTraits.toScmNumber(arguments.getHead());
    IList tail = arguments.getTail();

    boolean rising = true;
    boolean strict = true;
    boolean falling = true;

    do {
      ScmNumber next = ValueTraits.toScmNumber(tail.getHead());
      tail = tail.getTail();

      if (curr.isEqualTo(next)) {
        strict = false;
      } else {
        if (curr.isLessThan(next)) {
          falling = false;
        } else {
          rising = false;
        }

        if (!rising & !falling) {
          return false;
        }
      }

      curr = next;
    }
    while (!tail.isEmpty());

    return switch (mode) {
      case LT -> strict & rising;
      case LE -> rising;
      case EQ -> rising & falling;
      case GE -> falling;
      case GT -> strict & falling;
      default -> false;
    };
  }
}
