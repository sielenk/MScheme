package MScheme.functions;

import MScheme.util.Arity;
import MScheme.machine.Machine;
import MScheme.environment.DynamicEnvironment;
import MScheme.values.*;

import MScheme.exceptions.*;


final class Adder
    extends Reducer
{
    protected Value initial()
    { return ValueFactory.createNumber(0); }

    protected Value combine(Value fst, Value snd)
        throws NumberExpected
    { return fst.toNumber().plus(snd.toNumber()); }
}

final class Suber
    extends Reducer
{
    private final SchemeNumber _first;

    Suber(SchemeNumber first)
    { _first = first; }

    protected Value initial()
    { return _first; }

    protected Value combine(Value fst, Value snd)
        throws NumberExpected
    { return fst.toNumber().minus(snd.toNumber()); }
}

final class Multiplier
    extends Reducer
{
    protected Value initial()
    { return ValueFactory.createNumber(1); }

    protected Value combine(Value fst, Value snd)
        throws NumberExpected
    { return fst.toNumber().times(snd.toNumber()); }
}


final class Order {
    private final static Arity _arity = Arity.atLeast(2);

    public final static int LT = -2;
    public final static int LE = -1;
    public final static int EQ =  0;
    public final static int GE =  1;
    public final static int GT =  2;

    public final static boolean check(List arguments, int mode)
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
    public final static Value eq_3F(Value fst, Value snd)
    { return SchemeBoolean.create(fst.eq(snd)); }

    public final static Value eqv_3F(Value fst, Value snd)
    { return SchemeBoolean.create(fst.eqv(snd)); }

    public final static Value equal_3F(Value fst, Value snd)
    { return SchemeBoolean.create(fst.equal(snd)); }


    // 6.2 Numbers

    // 6.2.5 Numerical operations
    public final static Value number_3F(Value argument)
    { return SchemeBoolean.create(argument.isNumber()); }

    public final static Value complex_3F(Value argument)
    { return SchemeBoolean.create(argument.isNumber()); }

    public final static Value real_3F(Value argument)
    { return SchemeBoolean.create(argument.isNumber()); }

    public final static Value rational_3F(Value argument)
    { return SchemeBoolean.create(argument.isNumber()); }

    public final static Value integer_3F(Value argument)
    { return SchemeBoolean.create(argument.isNumber()); }


    public final static Value exact_3F(Value argument)
    { return SchemeBoolean.create(argument.isNumber()); }

    public final static Value inexact_3F(Value argument)
    { return SchemeBoolean.createFalse(); }


    public final static Value _3C(List arguments) // <
        throws RuntimeError, TypeError
    { return SchemeBoolean.create(Order.check(arguments, Order.LT)); }

    public final static Value _3C_3D(List arguments) // <=
        throws RuntimeError, TypeError
    { return SchemeBoolean.create(Order.check(arguments, Order.LE)); }

    public final static Value _3D(List arguments) // =
        throws RuntimeError, TypeError
    { return SchemeBoolean.create(Order.check(arguments, Order.EQ)); }

    public final static Value _3E_3D(List arguments) // >=
        throws RuntimeError, TypeError
    { return SchemeBoolean.create(Order.check(arguments, Order.GE)); }

    public final static Value _3E(List arguments) // >
        throws RuntimeError, TypeError
    { return SchemeBoolean.create(Order.check(arguments, Order.GT)); }


    private final static Adder ADDER = new Adder();

    public final static Value _2B(List arguments) // +
        throws RuntimeError, TypeError
    { return ADDER.reduceLeft(arguments); }


    private final static Arity AT_LEAST_1 = Arity.atLeast(1);

    public final static Value _2D(List arguments) // -
        throws RuntimeError, TypeError
    {
        int len = Function.checkArguments(AT_LEAST_1, arguments);

        SchemeNumber first = arguments.getHead().toNumber();
        if (len == 1) {
            return first.negated();
        } else {
            return new Suber(first).foldLeft(
                arguments.getTail()
            );
        }
    }

    private final static Multiplier MULTIPLITER = new Multiplier();

    public final static Value _2A(List arguments) // *
        throws RuntimeError, TypeError
    { return MULTIPLITER.reduceLeft(arguments); }

    // 6.3 Other data types

    // 6.3.1 Booleans

    public final static Value not(Value argument) // boolean?
    { return SchemeBoolean.create(!argument.isTrue()); }

    public final static Value boolean_3F(Value argument) // boolean?
    { return SchemeBoolean.create(argument.isBoolean()); }


    // 6.3.2 Pairs and lists

    public final static Value pair_3F(Value argument) // pair?
    { return SchemeBoolean.create(argument.isPair()); }

    public final static Value cons(Value fst, Value snd)
    { return Pair.create(fst, snd); }

    public final static Value car(Value argument)
        throws PairExpected
    { return argument.toPair().getFirst(); }

    public final static Value cdr(Value argument)
        throws PairExpected
    { return argument.toPair().getSecond(); }

    public final static Value set_2Dcar_21(Value fst, Value snd) // set-car!
        throws PairExpected, ImmutableException
    { fst.toPair().setFirst(snd); return snd; }

    public final static Value set_2Dcdr_21(Value fst, Value snd) // set-car!
        throws PairExpected, ImmutableException
    { fst.toPair().setSecond(snd); return snd; }


    public final static Value null_3F(Value argument) // pair?
    { return SchemeBoolean.create(argument.eq(Empty.create())); }

    public final static Value list_3F(Value argument) // list?
    { return SchemeBoolean.create(argument.isList()); }

    public final static Value list(List argument) // list?
    { return argument.toValue(); }

    public final static Value length(Value argument)
        throws ListExpected
    { return SchemeNumber.create(argument.toList().getLength()); }

    public final static Function append = AppendFunction.INSTANCE;

    public final static Value reverse(Value argument)
        throws ListExpected
    { return argument.toList().getReversed().toValue(); }

    public final static Function memq   = MemqFunction.INSTANCE;
    public final static Function memv   = MemvFunction.INSTANCE;
    public final static Function member = MemberFunction.INSTANCE;

    public final static Function assq  = AssqFunction.INSTANCE;
    public final static Function assv  = AssvFunction.INSTANCE;
    public final static Function assoc = AssocFunction.INSTANCE;


    // 6.3.3 Symbols
    public final static Value symbol_3F(Value argument) // symbol?
    { return SchemeBoolean.create(argument.isSymbol()); }

    public final static Value symbol_2D_3Estring(Value argument) // symbol->string
        throws SymbolExpected
    { return SchemeString.create(argument.toSymbol()); }

    public final static Value string_2D_3Esymbol(Value argument) // string->symbol
        throws StringExpected
    { return Symbol.create(argument.toScmString()); }


    // 6.3.4 Characters

    public final static Value char_3F(Value argument) // char?
    { return SchemeBoolean.create(argument.isChar()); }

    public final static Value char_3C_3F(Value fst, Value snd) // char<?
        throws CharExpected
    { return SchemeBoolean.create(fst.toChar().getJavaChar() < snd.toChar().getJavaChar()); }

    public final static Value char_3C_3D_3F(Value fst, Value snd) // char<=?
        throws CharExpected
    { return SchemeBoolean.create(fst.toChar().getJavaChar() <= snd.toChar().getJavaChar()); }

    public final static Value char_3D_3F(Value fst, Value snd) // char=?
        throws CharExpected
    { return SchemeBoolean.create(fst.toChar().getJavaChar() == snd.toChar().getJavaChar()); }

    public final static Value char_3E_3D_3F(Value fst, Value snd) // char>=?
        throws CharExpected
    { return SchemeBoolean.create(fst.toChar().getJavaChar() >= snd.toChar().getJavaChar()); }

    public final static Value char_3E_3F(Value fst, Value snd) // char>?
        throws CharExpected
    { return SchemeBoolean.create(fst.toChar().getJavaChar() > snd.toChar().getJavaChar()); }

    public final static Value char_2D_3Einteger(Value argument)
        throws CharExpected
    { return SchemeNumber.create(argument.toChar().getJavaChar()); }

    public final static Value integer_2D_3Echar(Value argument)
        throws NumberExpected
    { return SchemeChar.create((char)argument.toNumber().getInteger()); }

    public final static Value char_2Dupcase(Value argument)
        throws CharExpected
    { return SchemeChar.create(Character.toUpperCase(argument.toChar().getJavaChar())); }

    public final static Value char_2Ddowncase(Value argument)
        throws CharExpected
    { return SchemeChar.create(Character.toLowerCase(argument.toChar().getJavaChar())); }


    // 6.3.5 Strings

    public final static Value string_3F(Value argument) // string?
    { return SchemeBoolean.create(argument.isString()); }


    // 6.3.6 Vectors

    public final static Value vector_3F(Value argument) // vector?
    { return SchemeBoolean.create(argument.isVector()); }

    public final static Value vector(List arguments) // vector
        throws ListExpected
    { return SchemeVector.create(arguments); }

    public final static Value vector_2D_3Elist(Value argument) // vector->list
        throws VectorExpected
    { return argument.toVector().getList().toValue(); }

    public final static Value list_2D_3Evector(Value argument) // list->vector
        throws ListExpected
    { return SchemeVector.create(argument.toList()); }

    // 6.4 Control features

    public final static Value procedure_3F(Value argument) // procedure?
    { return SchemeBoolean.create(argument.isFunction()); }

    public final static Function apply = ApplyFunction.INSTANCE;

    public final static Function call_2Dwith_2Dcurrent_2Dcontinuation
        = CallCCFunction.INSTANCE;

    public final static Function dynamic_2Dwind = DynamicWindFunction.INSTANCE;


    // 6.5 Eval

    public final static Value eval(Value fst, Value snd)
        throws RuntimeError, TypeError
    {
        try {
            return new Machine(snd.toEnvironment()).evaluate(fst);
        }
        catch (CompileError e) {
            throw new RuntimeError(fst);
        }
    }

    public final static Value scheme_2Dreport_2Denvironment(Value fst)
        throws RuntimeError, TypeError
    {
        if (fst.toNumber().getInteger() != 5) {
            throw new RuntimeError(fst);
        }

        return DynamicEnvironment.getSchemeReportEnvironment();
    }

    public final static Value null_2Denvironment(Value fst)
        throws RuntimeError, TypeError
    {
        if (fst.toNumber().getInteger() != 5) {
            throw new RuntimeError(fst);
        }

        return DynamicEnvironment.getNullEnvironment();
    }


    // 6.6 Input and output

    // 6.6.1 Ports

    public final static Value port_3F(Value argument) // port?
    { return SchemeBoolean.create(argument.isPort()); }

    public final static Value input_2Dport_3F(Value argument) // input-port?
        throws PortExpected
    { return SchemeBoolean.create(argument.isPort() && argument.toPort().isInput()); }

    public final static Value output_2Dport_3F(Value argument) // output-port?
        throws PortExpected
    { return SchemeBoolean.create(argument.isPort() && argument.toPort().isOutput()); }


    public final static Value open_2Dinput_2Dfile(Value argument)
        throws StringExpected, OpenException
    { return InputPort.create(argument.toScmString()); }

    public final static Value open_2Doutput_2Dfile(Value argument)
        throws StringExpected, OpenException
    { return OutputPort.create(argument.toScmString()); }


    public final static Value close_2Dinput_2Dport(Value argument)
        throws PortExpected, CloseException
    { argument.toPort().toInput().close(); return argument; }
    
    public final static Value close_2Doutput_2Dport(Value argument)
        throws PortExpected, CloseException
    { argument.toPort().toOutput().close(); return argument; }
    

    // 6.6.2 Input

    public final static Value read(Value fst)
        throws RuntimeError, TypeError
    { return fst.toPort().toInput().read(); }

    public final static Value read_2Dchar(Value fst)
        throws RuntimeError, TypeError
    { return fst.toPort().toInput().readScmChar(); }

    public final static Value peek_2Dchar(Value fst)
        throws RuntimeError, TypeError
    { return fst.toPort().toInput().peekScmChar(); }

    public final static Value eof_2Dobject_3F(Value fst)
    { return SchemeBoolean.create(fst.eq(InputPort.EOF_VALUE)); }

    public final static Value char_2Dready(Value fst)
        throws TypeError
    { return SchemeBoolean.create(fst.toPort().toInput().isReady()); }


    // 6.6.3 Output

    public final static Value write(Value fst, Value snd)
        throws RuntimeError, TypeError
    { snd.toPort().toOutput().write(fst); return snd; }

    public final static Value display(Value fst, Value snd)
        throws RuntimeError, TypeError
    { snd.toPort().toOutput().display(fst); return snd; }

    public final static Value write_2Dchar(Value fst, Value snd)
        throws RuntimeError, TypeError
    { snd.toPort().toOutput().writeScmChar(fst.toChar()); return snd; }
}

