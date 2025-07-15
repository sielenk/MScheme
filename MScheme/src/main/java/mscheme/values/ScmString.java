/* The implementation of Scheme's strings.
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

package mscheme.values;

import java.io.IOException;
import java.io.Writer;
import mscheme.exceptions.ImmutableException;
import mscheme.exceptions.InvalidStringIndexException;


class ConstScmString
    extends ScmString {

  public final static String CVS_ID
      = "$Id$";

  private final String _string;

  public ConstScmString(String javaString) {
    _string = javaString;
  }

  public int getLength() {
    return _string.length();
  }

  public char get_(int index) {
    return _string.charAt(index);
  }

  public void set_(int index, char c)
      throws ImmutableException {
    throw new ImmutableException(this);
  }

  public String getJavaString() {
    return _string;
  }
}

class MutableScmString
    extends ScmString
    implements IMutable {

  public final static String CVS_ID
      = "$Id$";

  private final char[] _string;

  public MutableScmString(int size, char fill) {
    _string = new char[size];
    for (int i = 0; i < size; ++i) {
      _string[i] = fill;
    }
  }

  public MutableScmString(String javaString) {
    int size = javaString.length();
    _string = new char[size];
    javaString.getChars(0, size, _string, 0);
  }

  public int getLength() {
    return _string.length;
  }

  public char get_(int index) {
    return _string[index];
  }

  public void set_(int index, char c) {
    _string[index] = c;
  }

  public Object getConst() {
    return new ConstScmString(getJavaString());
  }

  public String getJavaString() {
    return new String(_string);
  }
}


public abstract class ScmString
    implements IComparable, IOutputable {

  public final static String CVS_ID
      = "$Id$";


  protected ScmString() {
  }

  public static ScmString create(int size, char fill) {
    return new MutableScmString(size, fill);
  }

  public static ScmString create(String javaString) {
    return new MutableScmString(javaString);
  }

  public static ScmString createConst(String javaString) {
    return new ConstScmString(javaString);
  }

  abstract public String getJavaString();

  // accessors

  private void validateIndex(int index)
      throws InvalidStringIndexException {
    if ((index < 0) || (getLength() <= index)) {
      throw new InvalidStringIndexException(this, index);
    }
  }

  public final void set(int index, char c)
      throws InvalidStringIndexException, ImmutableException {
    validateIndex(index);
    set_(index, c);
  }

  public final char get(int index)
      throws InvalidStringIndexException {
    validateIndex(index);
    return get_(index);
  }

  abstract public int getLength();

  abstract protected void set_(int index, char c)
      throws ImmutableException;

  abstract protected char get_(int index);

  public void outputOn(Writer destination, boolean doWrite) throws IOException {
    if (doWrite) {
      destination.write('"'); // "
      for (int i = 0; i < getLength(); i++) {
        char c = get_(i);
        switch (c) {
          case '\n':
            destination.write("\\n");
            break;

          case '"': // "
            destination.write("\\\"");
            break;

          default:
            destination.write(c);
            break;
        }
      }
      destination.write('"'); // "
    } else {
      destination.write(
          getJavaString());
    }
  }

  public boolean eq(Object other) {
    return this == other;
  }

  public boolean eqv(Object other) {
    return this == other;
  }

  public boolean equals(Object other) {
    if (!(other instanceof ScmString otherString)) {
      return false;
    }

    return getJavaString().compareTo(
        otherString.getJavaString()
    ) == 0;
  }
}
