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
      OutputPort out = OutputPort.Companion.create(outPipe);
      out.write(expected);
      out.close();
      outPipe.close();
    }

    Object actual = InputPort.Companion.create(inPipe).read();

    assertTrue(ValueTraits.INSTANCE.equal(expected, actual));
  }

  public void testReadWriteBoolean()
      throws Exception {
    checkReadWrite(ValueTraits.TRUE);
    checkReadWrite(ValueTraits.FALSE);
  }

  public void testReadWriteChar()
      throws Exception {
    checkReadWrite(ValueTraits.INSTANCE.toScmChar('a'));
    checkReadWrite(ValueTraits.INSTANCE.toScmChar('\n'));
    checkReadWrite(ValueTraits.INSTANCE.toScmChar(' '));
  }

  public void testReadWriteNumber()
      throws Exception {
    checkReadWrite(ValueTraits.INSTANCE.toScmNumber(-1));
    checkReadWrite(ValueTraits.INSTANCE.toScmNumber(0));
    checkReadWrite(ValueTraits.INSTANCE.toScmNumber(12));
  }

  public void testReadWriteList()
      throws Exception {
    checkReadWrite(ListFactory.INSTANCE.create());
    checkReadWrite(ListFactory.INSTANCE.create(ValueTraits.TRUE));
    checkReadWrite(ListFactory.INSTANCE.create(ValueTraits.INSTANCE.toScmChar('b'),
        ValueTraits.INSTANCE.toScmChar('\n')));
  }

  public void testReadWriteVector()
      throws Exception {
    checkReadWrite(ScmVector.Companion.create());
    checkReadWrite(ScmVector.Companion.create(new Object[]
        {ValueTraits.INSTANCE.toScmChar('b'), ValueTraits.INSTANCE.toScmChar('\n')}));
  }

  public void testReadWriteString()
      throws Exception {
    checkReadWrite(ScmString.Companion.create("Hallo World"));
    checkReadWrite(ScmString.Companion.create("And now the bad bits: \n \" \\ öäü"));
  }
}
