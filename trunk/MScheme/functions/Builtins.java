package MScheme.functions;

import MScheme.util.Arity;
import MScheme.exceptions.*;
import MScheme.values.*;


class Adder
    extends Reducer
{
    protected Value initial()
    { return ValueFactory.createNumber(0); }

    protected Value combine(Value fst, Value snd)
        throws NumberExpectedException
    { return fst.toNumber().plus(snd.toNumber()); }
}


class Multiplier
    extends Reducer
{
    protected Value initial()
    { return ValueFactory.createNumber(1); }

    protected Value combine(Value fst, Value snd)
        throws NumberExpectedException
    { return fst.toNumber().times(snd.toNumber()); }
}


class Order {
    private final static Arity _arity = Arity.atLeast(2);

    public final static int LT = -2;
    public final static int LE = -1;
    public final static int EQ =  0;
    public final static int GE =  1;
    public final static int GT =  2;

    public static boolean check(List arguments, int mode)
        throws RuntimeError, TypeError
    {
        int len = arguments.getLength();
        
        if (!_arity.isValid(len)) {
            throw new RuntimeArityError(arguments, _arity);
        }

        SchemeNumber curr = arguments.getHead().toNumber();
        List         tail = arguments.getTail();

        boolean rising  = true;
        boolean strict  = true;
        boolean falling = true;

        do {
            SchemeNumber next = tail.getHead().toNumber();
                         tail = tail.getTail();

            if (curr.isEqualTo(next)) {
                strict = false;
            } else {
                if (curr.isLessThan(next)) {
                    falling = false;
                } else {
                    rising  = false;
                }

                if (!rising & !falling) {
                    return false;
                }
            }

            curr = next;
        } while (!tail.isEmpty());

        switch (mode) {
        case LT: return strict & rising;
        case LE: return rising;
        case EQ: return rising & falling;
        case GE: return falling;
        case GT: return strict & falling;
        }

        return false; // unknown mode ...
    }
}


public class Builtins
{
    // 6. Standard procedures

    // 6.1 Equivalence predicates
    public static Value eq_3F(Value fst, Value snd)
    { return SchemeBoolean.create(fst.eq(snd)); }

    public static Value eqv_3F(Value fst, Value snd)
    { return SchemeBoolean.create(fst.eqv(snd)); }

    public static Value equal_3F(Value fst, Value snd)
    { return SchemeBoolean.create(fst.equal(snd)); }


    // 6.2 Numbers

    // 6.2.5 Numerical operations
    public static Value number_3F(Value argument)
    { return SchemeBoolean.create(argument.isNumber()); }

    public static Value complex_3F(Value argument)
    { return SchemeBoolean.create(argument.isNumber()); }

    public static Value real_3F(Value argument)
    { return SchemeBoolean.create(argument.isNumber()); }

    public static Value rational_3F(Value argument)
    { return SchemeBoolean.create(argument.isNumber()); }

    public static Value integer_3F(Value argument)
    { return SchemeBoolean.create(argument.isNumber()); }


    public static Value exact_3F(Value argument)
    { return SchemeBoolean.create(argument.isNumber()); }

    public static Value inexact_3F(Value argument)
    { return SchemeBoolean.createFalse(); }


    public static Value _3C(List arguments) // <
        throws RuntimeError, TypeError
    { return SchemeBoolean.create(Order.check(arguments, Order.LT)); }

    public static Value _3C_3D(List arguments) // <=
        throws RuntimeError, TypeError
    { return SchemeBoolean.create(Order.check(arguments, Order.LE)); }

    public static Value _3D(List arguments) // =
        throws RuntimeError, TypeError
    { return SchemeBoolean.create(Order.check(arguments, Order.EQ)); }

    public static Value _3E_3D(List arguments) // >=
        throws RuntimeError, TypeError
    { return SchemeBoolean.create(Order.check(arguments, Order.GE)); }

    public static Value _3E(List arguments) // >
        throws RuntimeError, TypeError
    { return SchemeBoolean.create(Order.check(arguments, Order.GT)); }


    private final static Adder ADDER = new Adder();

    public static Value _2B(List arguments) // +
        throws RuntimeError, TypeError
    { return ADDER.foldLeft(arguments); }

    private final static Multiplier MULTIPLITER = new Multiplier();

    public static Value _2A(List arguments) // *
        throws RuntimeError, TypeError
    { return MULTIPLITER.foldLeft(arguments); }

    // 6.3 Other data types

    // 6.3.1 Booleans

    public static Value not(Value argument) // boolean?
    { return SchemeBoolean.create(argument.isFalse()); }

    public static Value boolean_3F(Value argument) // boolean?
    { return SchemeBoolean.create(argument.isBoolean()); }


    // 6.3.2 Pairs and lists

    public static Value pair_3F(Value argument) // pair?
    { return SchemeBoolean.create(argument.isPair()); }

    public static Value cons(Value fst, Value snd)
    { return Pair.create(fst, snd); }

    public static Value car(Value argument)
        throws PairExpectedException
    { return argument.toPair().getFirst(); }

    public static Value cdr(Value argument)
        throws PairExpectedException
    { return argument.toPair().getSecond(); }

    public static Value set_2Dcar_21(Value fst, Value snd) // set-car!
        throws PairExpectedException, ImmutableException
    { fst.toPair().setFirst(snd); return snd; }

    public static Value set_2Dcdr_21(Value fst, Value snd) // set-car!
        throws PairExpectedException, ImmutableException
    { fst.toPair().setSecond(snd); return snd; }


    public static Value null_3F(Value argument) // pair?
    { return SchemeBoolean.create(argument.eq(List.with())); }

    public static Value list_3F(Value argument) // list?
    { return SchemeBoolean.create(argument.isList()); }

    public static Value list(List argument) // list?
    { return argument; }

    public static Value length(Value argument)
        throws ListExpectedException
    { return SchemeNumber.create(argument.toList().getLength()); }

    public static Value reverse(Value argument)
        throws ListExpectedException
    { return argument.toList().getReversed(); }


    // 6.3.3 Symbols
    public static Value symbol_3F(Value argument) // symbol?
    { return SchemeBoolean.create(argument.isSymbol()); }

    public static Value symbol_2D_3Estring(Value argument) // symbol->string
        throws SymbolExpectedException
    { return SchemeString.create(argument.toSymbol()); }

    public static Value string_2D_3Esymbol(Value argument) // string->symbol
        throws StringExpectedException
    { return Symbol.create(argument.toScmString()); }


    // 6.3.4 Characters

    public static Value char_3F(Value argument) // char?
    { return SchemeBoolean.create(argument.isChar()); }

    public static Value char_3C_3F(Value fst, Value snd) // char<?
        throws CharExpectedException
    { return SchemeBoolean.create(fst.toChar().getJavaChar() < snd.toChar().getJavaChar()); }

    public static Value char_3C_3D_3F(Value fst, Value snd) // char<=?
        throws CharExpectedException
    { return SchemeBoolean.create(fst.toChar().getJavaChar() <= snd.toChar().getJavaChar()); }

    public static Value char_3D_3F(Value fst, Value snd) // char=?
        throws CharExpectedException
    { return SchemeBoolean.create(fst.toChar().getJavaChar() == snd.toChar().getJavaChar()); }

    public static Value char_3E_3D_3F(Value fst, Value snd) // char>=?
        throws CharExpectedException
    { return SchemeBoolean.create(fst.toChar().getJavaChar() >= snd.toChar().getJavaChar()); }

    public static Value char_3E_3F(Value fst, Value snd) // char>?
        throws CharExpectedException
    { return SchemeBoolean.create(fst.toChar().getJavaChar() > snd.toChar().getJavaChar()); }

    public static Value char_2D_3Einteger(Value argument)
        throws CharExpectedException
    { return SchemeNumber.create(argument.toChar().getJavaChar()); }

    public static Value integer_2D_3Echar(Value argument)
        throws NumberExpectedException
    { return SchemeChar.create((char)argument.toNumber().getInteger()); }

    public static Value char_2Dupcase(Value argument)
        throws CharExpectedException
    { return SchemeChar.create(Character.toUpperCase(argument.toChar().getJavaChar())); }

    public static Value char_2Ddowncase(Value argument)
        throws CharExpectedException
    { return SchemeChar.create(Character.toLowerCase(argument.toChar().getJavaChar())); }


    // 6.3.5 Strings

    public static Value string_3F(Value argument) // string?
    { return SchemeBoolean.create(argument.isString()); }


    // 6.3.6 Vectors

    public static Value vector_3F(Value argument) // vector?
    { return SchemeBoolean.create(argument.isVector()); }


    // 6.4 Control features

    public static Value procedure_3F(Value argument) // procedure?
    { return SchemeBoolean.create(argument.isFunction()); }


    // 6.6 Input and output

    // 6.6.1 Ports

    public static Value port_3F(Value argument) // port?
    { return SchemeBoolean.create(argument.isPort()); }

    public static Value input_2Dport_3F(Value argument) // input-port?
        throws PortExpectedException
    { return SchemeBoolean.create(argument.isPort() && argument.toPort().isInput()); }

    public static Value output_2Dport_3F(Value argument) // output-port?
        throws PortExpectedException
    { return SchemeBoolean.create(argument.isPort() && argument.toPort().isOutput()); }


    public static Value open_2Dinput_2Dfile(Value argument)
        throws StringExpectedException, OpenException
    { return InputPort.create(argument.toScmString()); }

    public static Value open_2Doutput_2Dfile(Value argument)
        throws StringExpectedException, OpenException
    { return OutputPort.create(argument.toScmString()); }


    public static Value close_2Dinput_2Dport(Value argument)
        throws PortExpectedException, CloseException
    { argument.toPort().toInput().close(); return argument; }
    
    public static Value close_2Doutput_2Dport(Value argument)
        throws PortExpectedException, CloseException
    { argument.toPort().toOutput().close(); return argument; }
    

    // 6.6.2 Input

    public static Value read(Value fst)
        throws SchemeException
    { return fst.toPort().toInput().read(); }

    public static Value read_2Dchar(Value fst)
        throws SchemeException
    { return fst.toPort().toInput().readScmChar(); }

    public static Value peek_2Dchar(Value fst)
        throws SchemeException
    { return fst.toPort().toInput().peekScmChar(); }

    public static Value eof_2Dobject_3F(Value fst)
    { return SchemeBoolean.create(fst.eq(InputPort.EOF_VALUE)); }

    public static Value char_2Dready(Value fst)
        throws SchemeException
    { return SchemeBoolean.create(fst.toPort().toInput().isReady()); }


    // 6.6.3 Output

    public static Value write(Value fst, Value snd)
        throws SchemeException
    { snd.toPort().toOutput().write(fst); return snd; }

    public static Value display(Value fst, Value snd)
        throws SchemeException
    { snd.toPort().toOutput().display(fst); return snd; }

    public static Value write_2Dchar(Value fst, Value snd)
        throws SchemeException
    { snd.toPort().toOutput().writeScmChar(fst.toChar()); return snd; }
}

