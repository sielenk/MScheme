/* Collected implementations of various functions.
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

package mscheme.values.functions;

import java.io.StringReader;

import mscheme.environment.Environment;

import mscheme.exceptions.CharExpected;
import mscheme.exceptions.CloseException;
import mscheme.exceptions.ImmutableException;
import mscheme.exceptions.InvalidStringIndexException;
import mscheme.exceptions.ListExpected;
import mscheme.exceptions.NumberExpected;
import mscheme.exceptions.OpenException;
import mscheme.exceptions.OutputPortExpected;
import mscheme.exceptions.PairExpected;
import mscheme.exceptions.PortExpected;
import mscheme.exceptions.RuntimeArityError;
import mscheme.exceptions.RuntimeError;
import mscheme.exceptions.SchemeException;
import mscheme.exceptions.StringExpected;
import mscheme.exceptions.SymbolExpected;
import mscheme.exceptions.TypeError;
import mscheme.exceptions.VectorException;
import mscheme.exceptions.VectorExpected;

import mscheme.util.Arity;

import mscheme.values.Function;
import mscheme.values.InputPort;
import mscheme.values.List;
import mscheme.values.Empty;
import mscheme.values.ListFactory;
import mscheme.values.OutputPort;
import mscheme.values.ScmNumber;
import mscheme.values.ScmString;
import mscheme.values.ScmVector;
import mscheme.values.ValueTraits;


final class Order
{
    public final static String id
        = "$Id$";


    public final static int LT = -2;
    public final static int LE = -1;
    public final static int EQ =  0;
    public final static int GE =  1;
    public final static int GT =  2;

    public final static boolean check(List arguments, int mode)
        throws RuntimeError, TypeError
    {
        final Arity arity = Arity.atLeast(2);
        int len = arguments.getLength();

        if (!arity.isValid(len))
        {
            throw new RuntimeArityError(arguments, arity);
        }

        ScmNumber curr = ValueTraits.toScmNumber(arguments.getHead());
        List      tail = arguments.getTail();

        boolean rising  = true;
        boolean strict  = true;
        boolean falling = true;

        do
        {
            ScmNumber next = ValueTraits.toScmNumber(tail.getHead());
            tail = tail.getTail();

            if (curr.isEqualTo(next))
            {
                strict = false;
            }
            else
            {
                if (curr.isLessThan(next))
                {
                    falling = false;
                }
                else
                {
                    rising  = false;
                }

                if (!rising & !falling)
                {
                    return false;
                }
            }

            curr = next;
        }
        while (!tail.isEmpty());

        switch (mode)
        {
        case LT:
            return strict & rising;
        case LE:
            return rising;
        case EQ:
            return rising & falling;
        case GE:
            return falling;
        case GT:
            return strict & falling;
        }

        return false; // unknown mode ...
    }
}


public class Builtins
{
    public final static String id
        = "$Id$";


    // 6. Standard procedures

    // 6.1 Equivalence predicates
    public final static Object eq_3F(Object fst, Object snd)
    {
        return ValueTraits.toScmBoolean(ValueTraits.eq(fst, snd));
    }

    public final static Object eqv_3F(Object fst, Object snd)
    {
		return ValueTraits.toScmBoolean(ValueTraits.eqv(fst, snd));
    }

    public final static Object equal_3F(Object fst, Object snd)
    {
		return ValueTraits.toScmBoolean(ValueTraits.equal(fst, snd));
    }


    // 6.2 Numbers

    // 6.2.5 Numerical operations
    public final static Object number_3F(Object argument)
    {
        return ValueTraits.toScmBoolean(ValueTraits.isScmNumber(argument));
    }

    public final static Object complex_3F(Object argument)
    {
		return ValueTraits.toScmBoolean(ValueTraits.isScmNumber(argument));
    }

    public final static Object real_3F(Object argument)
    {
		return ValueTraits.toScmBoolean(ValueTraits.isScmNumber(argument));
    }

    public final static Object rational_3F(Object argument)
    {
		return ValueTraits.toScmBoolean(ValueTraits.isScmNumber(argument));
    }

    public final static Object integer_3F(Object argument)
    {
		return ValueTraits.toScmBoolean(ValueTraits.isScmNumber(argument));
    }


    public final static Object exact_3F(Object argument)
    {
		return ValueTraits.toScmBoolean(ValueTraits.isScmNumber(argument));
    }

    public final static Object inexact_3F(Object argument)
    {
        return ValueTraits.FALSE;
    }


    public final static Object _3C(List arguments) // <
    throws RuntimeError, TypeError
    {
        return ValueTraits.toScmBoolean(Order.check(arguments, Order.LT));
    }

    public final static Object _3C_3D(List arguments) // <=
    throws RuntimeError, TypeError
    {
        return ValueTraits.toScmBoolean(Order.check(arguments, Order.LE));
    }

    public final static Object _3D(List arguments) // =
    throws RuntimeError, TypeError
    {
        return ValueTraits.toScmBoolean(Order.check(arguments, Order.EQ));
    }

    public final static Object _3E_3D(List arguments) // >=
    throws RuntimeError, TypeError
    {
        return ValueTraits.toScmBoolean(Order.check(arguments, Order.GE));
    }

    public final static Object _3E(List arguments) // >
    throws RuntimeError, TypeError
    {
        return ValueTraits.toScmBoolean(Order.check(arguments, Order.GT));
    }


    public final static Object zero_3F(Object argument) // zero?
        throws TypeError
    {
        return ValueTraits.toScmBoolean(ValueTraits.toScmNumber(argument).getInteger() == 0);
    }


    public final static Object _2B(List arguments) // +
        throws RuntimeError, TypeError
    {
        ScmNumber sum  = ScmNumber.create(0);
        List      tail = arguments;
            
        while (!tail.isEmpty())
        {
            ScmNumber term     = ValueTraits.toScmNumber(tail.getHead());
            List      nextTail = tail.getTail();

            sum  = sum.plus(term);
            tail = nextTail;
        }
        return sum;
    }

    public final static Object _2D(List arguments) // -
        throws RuntimeError, TypeError
    {
        ScmNumber result = ValueTraits.toScmNumber(arguments.getHead());
        List      rest   = arguments.getTail();

        if (!rest.isEmpty())
        {
            do
            {
                ScmNumber head = ValueTraits.toScmNumber(rest.getHead());
                List      tail = rest.getTail();

                result = result.minus(head);
                rest   = tail;
            }
            while (!rest.isEmpty());

            return result;
        }
        else
        {
            return result.negated();
        }
    }

    public final static Object _2A(List arguments) // *
        throws RuntimeError, TypeError
    {
        ScmNumber product = ScmNumber.create(1);
        List      tail    = arguments;

        while (!tail.isEmpty())
        {
            ScmNumber factor   = ValueTraits.toScmNumber(tail.getHead());
            List      nextTail = tail.getTail();
                
            product = product.times(factor);
            tail    = nextTail;
        }

        return product;
    }

    public final static Object _2F(List arguments) // /
        throws RuntimeError, TypeError
    {
        ScmNumber result = ValueTraits.toScmNumber(arguments.getHead());
        List      rest   = arguments.getTail();

        if (!rest.isEmpty())
        {
            do
            {
                ScmNumber head = ValueTraits.toScmNumber(rest.getHead());
                List      tail = rest.getTail();

                result = result.divide(head);
                rest   = tail;
            }
            while (!rest.isEmpty());

            return result;
        }
        else
        {
            return result.reciprocal();
        }
    }


    // 6.3 Other data types

    // 6.3.1 Booleans

    public final static Object not(Object argument)
    {
        return ValueTraits.toScmBoolean(!ValueTraits.isTrue(argument));
    }

    public final static Object boolean_3F(Object argument) // boolean?
    {
        return ValueTraits.toScmBoolean(
			ValueTraits.isScmBoolean(argument));
    }


    // 6.3.2 Pairs and lists

    public final static Object pair_3F(Object argument) // pair?
    {
        return ValueTraits.toScmBoolean(ValueTraits.isPair(argument));
    }

    public final static Object cons(Object fst, Object snd)
    {
        return ListFactory.createPair(fst, snd);
    }

    public final static Object car(Object argument)
    throws PairExpected
    {
        return ValueTraits.toPair(argument).getFirst();
    }

    public final static Object cdr(Object argument)
    throws PairExpected
    {
        return ValueTraits.toPair(argument).getSecond();
    }

    public final static Object set_2Dcar_21(Object fst, Object snd) // set-car!
    throws PairExpected, ImmutableException
    {
		ValueTraits.toPair(fst).setFirst(snd);
        return snd;
    }

    public final static Object set_2Dcdr_21(Object fst, Object snd) // set-car!
    throws PairExpected, ImmutableException
    {
		ValueTraits.toPair(fst).setSecond(snd);
        return snd;
    }


    public final static Object null_3F(Object argument) // null?
    {
        return ValueTraits.toScmBoolean(argument instanceof Empty);
    }

    public final static Object list_3F(Object argument) // list?
    {
        return ValueTraits.toScmBoolean(ValueTraits.isList(argument));
    }

    public final static Object list(List argument)
        throws ListExpected
    {
        // Without first-class continuations, it would be save to
        // omit the call to getCopy(). But multiple returns have to
        // return different lists ...
        return argument.getCopy();
    }

    public final static Object length(Object argument)
        throws ListExpected
    {
        return ScmNumber.create(ValueTraits.toList(argument).getLength());
    }

    public final static Function append = AppendFunction.INSTANCE;

    public final static Object reverse(Object argument)
        throws ListExpected
    {
        return ValueTraits.toList(argument).getReversed();
    }

    public final static Function memq   = MemqFunction.INSTANCE;
    public final static Function memv   = MemvFunction.INSTANCE;
    public final static Function member = MemberFunction.INSTANCE;

    public final static Function assq  = AssqFunction.INSTANCE;
    public final static Function assv  = AssvFunction.INSTANCE;
    public final static Function assoc = AssocFunction.INSTANCE;


    // 6.3.3 Symbols
    public final static Object symbol_3F(Object argument) // symbol?
    {
        return ValueTraits.toScmBoolean(ValueTraits.isSymbol(argument));
    }

    public final static Object symbol_2D_3Estring(Object argument) // symbol->string
    throws SymbolExpected
    {
        return ScmString.create(ValueTraits.toSymbol(argument));
    }

    public final static Object string_2D_3Esymbol(Object argument) // string->symbol
    throws StringExpected
    {
        return ScmString.toString(ValueTraits.toScmString(argument)).intern();
    }


    // 6.3.4 Characters

    public final static Object char_3F(Object argument) // char?
    {
        return ValueTraits.toScmBoolean(ValueTraits.isScmChar(argument));
    }

    public final static Object char_3C_3F(Object fst, Object snd) // char<?
    throws CharExpected
    {
        return ValueTraits.toScmBoolean(
        	ValueTraits.toScmChar(fst).charValue() 
        	< ValueTraits.toScmChar(snd).charValue());
    }

    public final static Object char_3C_3D_3F(Object fst, Object snd) // char<=?
    throws CharExpected
    {
        return ValueTraits.toScmBoolean(
			ValueTraits.toScmChar(fst).charValue()
			<= ValueTraits.toScmChar(snd).charValue());
    }

    public final static Object char_3D_3F(Object fst, Object snd) // char=?
    throws CharExpected
    {
        return ValueTraits.toScmBoolean(
			ValueTraits.toScmChar(fst).charValue()
			== ValueTraits.toScmChar(snd).charValue());
    }

    public final static Object char_3E_3D_3F(Object fst, Object snd) // char>=?
    throws CharExpected
    {
        return ValueTraits.toScmBoolean(
        	ValueTraits.toScmChar(fst).charValue()
        	>= ValueTraits.toScmChar(snd).charValue());
    }

    public final static Object char_3E_3F(Object fst, Object snd) // char>?
    throws CharExpected
    {
        return ValueTraits.toScmBoolean(
        	ValueTraits.toScmChar(fst).charValue() 
        	> ValueTraits.toScmChar(snd).charValue());
    }

    public final static Object char_2D_3Einteger(Object argument)
    throws CharExpected
    {
        return ScmNumber.create(
        	ValueTraits.toScmChar(argument).charValue());
    }

    public final static Object integer_2D_3Echar(Object argument)
    throws NumberExpected
    {
        return ValueTraits.toScmChar(
        	(char)ValueTraits.toScmNumber(argument).getInteger());
    }

    public final static Object char_2Dupcase(Object argument)
    throws CharExpected
    {
        return ValueTraits.toScmChar(
        	Character.toUpperCase(
        		ValueTraits.toScmChar(argument).charValue()));
    }

    public final static Object char_2Ddowncase(Object argument)
    throws CharExpected
    {
        return ValueTraits.toScmChar(
        	Character.toLowerCase(
        		ValueTraits.toScmChar(argument).charValue()));
    }


    // 6.3.5 Strings

    public final static Object string_3F(Object argument) // string?
    {
        return ValueTraits.toScmBoolean(ValueTraits.isScmString(argument));
    }

    public final static Object make_2Dstring(Object k, Object c)
    throws TypeError
    {
        return ScmString.create(
			ValueTraits.toScmNumber(k).getInteger(),
			ValueTraits.toScmChar(c).charValue());
    }

    public final static Object string_2Dlength(Object str)
    throws TypeError
    {
        return ScmNumber.create(
        	ScmString.getLength(
        		ValueTraits.toScmString(
        			str)));
    }

    public final static Object string_2Dref(Object str, Object k)
    throws TypeError, InvalidStringIndexException
    {
        return ValueTraits.toScmChar(
			ScmString.get(
				ValueTraits
				  .toScmString(str),
				ValueTraits
				  .toScmNumber(k)
				  .getInteger()));
    }

    public final static Object string_2Dset_21(Object str, Object k, Object c)
    throws TypeError, InvalidStringIndexException, ImmutableException
    {
    	ScmString.set(
			ValueTraits.toScmString(str),
			ValueTraits.toScmNumber(k).getInteger(),
			ValueTraits.toScmChar(c).charValue());

        return c;
    }

    public final static Object string_2Dappend(List arguments)
    throws TypeError, InvalidStringIndexException, ImmutableException
    {
        StringBuffer accu = new StringBuffer();
        
        for (
            List rest = arguments;
            !rest.isEmpty();
            rest = rest.getTail()
        )
        {
            accu.append(
            	ScmString.toString(
					ValueTraits.toScmString(
						rest.getHead())));
        }

        return ScmString.create(
            accu.toString()
        );
    }

    public final static Object string_2Dcopy(Object string)
    throws TypeError, InvalidStringIndexException, ImmutableException
    {
        return ScmString.create(
        	ScmString.toString(
				ValueTraits.toScmString(
					string)));
    }

    public final static Object string_2D_3Elist(Object scmString)
    throws TypeError, InvalidStringIndexException, ImmutableException
    {
        String javaString = ScmString.toString(ValueTraits.toScmString(scmString));
        List   result     = ListFactory.create();

        for (
            int i = javaString.length() - 1;
            i >= 0;
            --i
        )
        {
            result = ListFactory.prepend(
				ValueTraits.toScmChar(
                    javaString.charAt(i)),
                result
            );
        }

        return result;
    }

    public final static Object list_2D_3Estring(Object list)
    throws TypeError, InvalidStringIndexException, ImmutableException
    {
        StringBuffer accu = new StringBuffer();
        
        for (
            List rest = ValueTraits.toList(list);
            !rest.isEmpty();
            rest = rest.getTail()
        )
        {
            accu.append(
				ValueTraits.toScmChar(
					rest.getHead())
                .charValue());
        }
        
        return ScmString.create(
            accu.toString()
        );
    }

    // 6.3.6 Vectors

    public final static Object vector_3F(Object argument) // vector?
    {
        return ValueTraits.toScmBoolean(ValueTraits.isScmVector(argument));
    }

    public final static Object make_2Dvector(Object k, Object obj)
    throws TypeError
    {
        return ScmVector.create(
			ValueTraits.toScmNumber(k).getInteger(),
			obj);
    }

    public final static Object vector_2Dlength(Object str)
    throws TypeError
    {
        return ScmNumber.create(
			ScmVector.getLength(
				ValueTraits.toScmVector(str)));
    }

    public final static Object vector_2Dref(Object vector, Object k)
    throws TypeError, VectorException
    {
        return ScmVector.get(
        	ValueTraits.toScmVector(vector),
			ValueTraits.toScmNumber(k).getInteger());
    }

    public final static Object vector_2Dset_21(Object vector, Object k, Object obj)
    throws TypeError, VectorException, ImmutableException
    {
		ScmVector.set(
			ValueTraits.toScmVector(vector),
			ValueTraits.toScmNumber(k).getInteger(),
            obj);

        return obj;
    }

    public final static Object vector(List arguments) // vector
    throws ListExpected
    {
        return ScmVector.create(arguments);
    }


    public final static Object vector_2D_3Elist(Object argument) // vector->list
    throws VectorExpected
    {
        return ScmVector.getList(ValueTraits.toScmVector(argument));
    }

    public final static Object list_2D_3Evector(Object argument) // list->vector
    throws ListExpected
    {
        return ScmVector.create(ValueTraits.toList(argument));
    }

    // 6.4 Control features

    public final static Object procedure_3F(Object argument) // procedure?
    {
        return ValueTraits.toScmBoolean(ValueTraits.isFunction(argument));
    }

    public final static Function apply = ApplyFunction.INSTANCE;

    public final static Function call_2Dwith_2Dcurrent_2Dcontinuation
        = CallCCFunction.INSTANCE;

//  public final static Function dynamic_2Dwind = DynamicWindFunction.INSTANCE;


    // 6.5 Eval

    public final static Function eval = EvalFunction.INSTANCE;

    public final static Object scheme_2Dreport_2Denvironment(Object fst)
        throws RuntimeError, TypeError
    {
        if (ValueTraits.toScmNumber(fst).getInteger() != 5)
        {
            throw new RuntimeError(fst);
        }

        return Environment.getSchemeReportEnvironment();
    }

    public final static Object null_2Denvironment(Object fst)
        throws RuntimeError, TypeError
    {
        if (ValueTraits.toScmNumber(fst).getInteger() != 5)
        {
            throw new RuntimeError(fst);
        }

        return Environment.getNullEnvironment();
    }


    // 6.6 Input and output

    // 6.6.1 Ports

    public final static Object port_3F(Object argument) // port?
    {
        return ValueTraits.toScmBoolean(ValueTraits.isPort(argument));
    }

    public final static Object input_2Dport_3F(Object argument) // input-port?
    throws PortExpected
    {
        return ValueTraits.toScmBoolean(argument instanceof InputPort);
    }

    public final static Object output_2Dport_3F(Object argument) // output-port?
    throws PortExpected
    {
        return ValueTraits.toScmBoolean(argument instanceof OutputPort);
    }


    public final static Object open_2Dinput_2Dfile(Object argument)
    throws StringExpected, OpenException
    {
        return InputPort.create(
        	ScmString.toString(
        		ValueTraits.toScmString(
        			argument)));
    }

    public final static Object open_2Doutput_2Dfile(Object argument)
    throws StringExpected, OpenException
    {
        return OutputPort.create(
        	ScmString.toString(
        		ValueTraits.toScmString(
        			argument)));
    }


    public final static Object close_2Dinput_2Dport(Object argument)
    throws PortExpected, CloseException
    {
		ValueTraits.toInputPort(argument).close();
        return argument;
    }

    public final static Object close_2Doutput_2Dport(Object argument)
    throws PortExpected, CloseException
    {
		ValueTraits.toOutputPort(argument).close();
        return argument;
    }


    // 6.6.2 Input

    public final static Object read(Object fst)
    throws RuntimeError, TypeError
    {
        return ValueTraits.toInputPort(fst).read();
    }

    public final static Object read_2Dchar(Object fst)
    throws RuntimeError, TypeError
    {
        return ValueTraits.toInputPort(fst).readScmChar();
    }

    public final static Object peek_2Dchar(Object fst)
    throws RuntimeError, TypeError
    {
        return ValueTraits.toInputPort(fst).peekScmChar();
    }

    public final static Object eof_2Dobject_3F(Object fst)
    {
        return ValueTraits.toScmBoolean(ValueTraits.eq(fst, InputPort.EOF_VALUE));
    }

    public final static Object char_2Dready_3F(Object fst)
    throws TypeError
    {
        return ValueTraits.toScmBoolean(ValueTraits.toInputPort(fst).isReady());
    }


    // 6.6.3 Output

    public final static Object write(Object fst, Object snd)
    throws OutputPortExpected, SchemeException
    {
		ValueTraits.toOutputPort(snd).write(fst);
        return snd;
    }

    public final static Object display(Object fst, Object snd)
    throws OutputPortExpected, SchemeException
    {
		ValueTraits.toOutputPort(snd).display(fst);
        return snd;
    }

    public final static Object write_2Dchar(Object fst, Object snd)
    throws RuntimeError, TypeError
    {
		ValueTraits.toOutputPort(snd).writeScmChar(
			ValueTraits.toScmChar(fst));
        return snd;
    }




    // additional functions
    
    public final static Object __unique_2Did()
    {
        return ValueTraits.createUniqueSymbol();
    }

    public final static Function
        __spawn = SpawnFunction.INSTANCE;

    public final static Function
        __y_2Dcombinator = YCombinator.INSTANCE;
    
    public final static Object __open_2Dinput_2Dstring(Object argument)
        throws TypeError
    {
        return InputPort.create(
            new StringReader(
				ScmString.toString(
					ValueTraits.toScmString(argument))));
    }

//  not very usefull yet ... needs GET-OUTPUT-STRING
//  public final static Object __open_2Doutput_2Dstring()
//      throws TypeError
//  {
//      return OutputPort.create(
//          new StringWriter()
//      );
//  }
}
