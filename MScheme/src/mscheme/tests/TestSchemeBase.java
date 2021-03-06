/* 
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

package mscheme.tests;

import junit.framework.TestCase;

import mscheme.exceptions.SchemeException;

import mscheme.machine.Machine;
import mscheme.values.ValueTraits;


public abstract class TestSchemeBase
    extends TestCase
{
    public final static String CVS_ID
        = "$Id$";

    private Machine machine;


    public TestSchemeBase(String name)
    {
        super(name);
    }


    protected void setUp()
        throws Exception
    {
        machine = new Machine();
    }

    protected void tearDown()
    {
        machine = null;
    }


    public Object quote(String expression)
        throws SchemeException, InterruptedException
    {
        return Machine.parse(expression);
    }

    public Object eval(String expression)
        throws SchemeException, InterruptedException
    {
        return machine.evaluate(expression);
    }

    public void check(String in, String out)
        throws SchemeException, InterruptedException
    {
        Object  value   = eval(in);
        boolean success = ValueTraits.equal(value, quote(out));

        if (!success)
        {
            System.out.println(
                "*** evaluation of ***\n" +
                in + '\n' +
                "*** returned ***\n" +
                value + '\n' +
                "*** expected was ***\n" +
                out + '\n' +
                "*** end ***"
            );
        }

        assertTrue(success);
    }
}
