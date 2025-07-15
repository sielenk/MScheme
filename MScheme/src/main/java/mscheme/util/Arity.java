/* A general purpose arity tester.
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

package mscheme.util;

import mscheme.exceptions.RuntimeArityError;

public class Arity {

  public final static String CVS_ID
      = "$Id$";


  private final int _minArity;
  private final int _maxArity;

  private Arity(int minArity, int maxArity) {
    _minArity = minArity;
    _maxArity = maxArity;
  }


  public static Arity exactly(int arity) {
    return new Arity(arity, arity);
  }

  public static Arity atLeast(int arity) {
    return new Arity(arity, -1);
  }

  public static Arity inRange(int lo, int hi) {
    return new Arity(lo, hi);
  }


  public Arity getOneLess()
      throws RuntimeArityError {
    int newMin = getMin() - 1;

    if (newMin < 0) {
      newMin = 0;
    }

    if (allowMore()) {
      return atLeast(newMin);
    } else {
      int newMax = getMax() - 1;

      if (newMax < 0) {
        throw new RuntimeArityError(null, this);
      }

      return inRange(newMin, newMax);
    }
  }


  public int getMin() {
    return _minArity;
  }

  public int getMax() {
    return _maxArity;
  }

  public boolean allowMore() {
    return (_maxArity == -1);
  }


  public boolean isValid(int arity) {
    boolean gotEnoughArguments = (_minArity <= arity);
    boolean isMaxArityDisabled = (_maxArity == -1);
    boolean gotTooManyArguments = !isMaxArityDisabled && (_maxArity < arity);

    return (gotEnoughArguments && !gotTooManyArguments);
  }


  public String toString() {
    String result = "" + getMin();

    if (allowMore()) {
      result += " or more";
    } else if (getMin() != getMax()) {
      result += " to " + getMax();
    }

    return result;
  }
}
