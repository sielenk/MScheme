/*
 * Created on 03.01.2004
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package mscheme.values;

import java.io.PipedReader;
import java.io.PipedWriter;
import junit.framework.TestCase;

/**
 * @author sielenk
 *
 * To change the template for this generated type comment go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
public class OutputPortTest
    extends TestCase {

  public OutputPortTest(String name) {
    super(name);
  }

  private static void checkReadWrite(Object expected)
      throws Exception {
    PipedReader inPipe = new PipedReader();

    {
      PipedWriter outPipe = new PipedWriter(inPipe);
      OutputPort out = OutputPort.create(outPipe);
      out.write(expected);
      out.close();
      outPipe.close();
    }

    Object actual = InputPort.create(inPipe).read();

    assertTrue(ValueTraits.equal(expected, actual));
  }

  public void testReadWriteBoolean()
      throws Exception {
    checkReadWrite(ValueTraits.TRUE);
    checkReadWrite(ValueTraits.FALSE);
  }

  public void testReadWriteChar()
      throws Exception {
    checkReadWrite(ValueTraits.toScmChar('a'));
    checkReadWrite(ValueTraits.toScmChar('\n'));
    checkReadWrite(ValueTraits.toScmChar(' '));
  }

  public void testReadWriteNumber()
      throws Exception {
    checkReadWrite(ValueTraits.toScmNumber(-1));
    checkReadWrite(ValueTraits.toScmNumber(0));
    checkReadWrite(ValueTraits.toScmNumber(12));
  }

  public void testReadWriteList()
      throws Exception {
    checkReadWrite(ListFactory.create());
    checkReadWrite(ListFactory.create(ValueTraits.TRUE));
    checkReadWrite(ListFactory.create(ValueTraits.toScmChar('b'),
        ValueTraits.toScmChar('\n')));
  }

  public void testReadWriteVector()
      throws Exception {
    checkReadWrite(ScmVector.create());
    checkReadWrite(ScmVector.create(new Object[]
        {ValueTraits.toScmChar('b'), ValueTraits.toScmChar('\n')}));
  }

  public void testReadWriteString()
      throws Exception {
    checkReadWrite(ScmString.create("Hallo World"));
    checkReadWrite(ScmString.create("And now the bad bits: \n \" \\ öäü"));
  }
}
