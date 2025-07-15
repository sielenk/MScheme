/* The implementation of Scheme's output ports.
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

import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import mscheme.exceptions.CloseException;
import mscheme.exceptions.OpenException;
import mscheme.exceptions.WriteException;


public class OutputPort
    extends Port {

  public final static String CVS_ID
      = "$Id$";


  private final Writer _writer;

  private OutputPort(Writer writer) {
    _writer = writer;
  }


  public static OutputPort create(Writer writer) {
    return new OutputPort(writer);
  }

  public static OutputPort create(ScmString filename)
      throws OpenException {
    return create(filename.getJavaString());
  }

  public static OutputPort create(String filename)
      throws OpenException {
    try {
      return create(new FileWriter(filename));
    } catch (IOException e) {
      throw new OpenException(
          ScmString.create(filename)
      );
    }
  }

  // specialisation of Port

  public void writeOn(Writer destination)
      throws IOException {
    destination.write("#[output port]");
  }

  public OutputPort toOutputPort() {
    return this;
  }


  public void close()
      throws CloseException {
    try {
      _writer.close();
    } catch (IOException e) {
      throw new CloseException(this);
    }
  }

  // output port

  public void writeChar(char c)
      throws WriteException {
    try {
      _writer.write(c);
      _writer.flush();
    } catch (IOException e) {
      throw new WriteException(this);
    }
  }

  public void writeScmChar(Character c)
      throws WriteException {
    writeChar(c);
  }

  public void write(Object datum)
      throws WriteException {
    try {
      ValueTraits.write(_writer, datum);
      _writer.flush();
    } catch (IOException e) {
      throw new WriteException(this);
    }
  }

  public void display(Object datum)
      throws WriteException {
    try {
      ValueTraits.display(_writer, datum);
      _writer.flush();
    } catch (IOException e) {
      throw new WriteException(this);
    }
  }
}
