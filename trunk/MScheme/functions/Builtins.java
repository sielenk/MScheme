package MScheme.functions;

import MScheme.util.Arity;
import MScheme.machine.Machine;
import MScheme.environment.DynamicEnvironment;
import MScheme.values.*;

import MScheme.exceptions.*;


final class Adder
    extends Reducer
{
    Adder()
    { super(ScmNumber.create(0)); }

    protected Value combine(Value fst, Value snd)
        throws NumberExpected
    { return fst.toScmNumber().plus(snd.toScmNumber()); }
}

final class Suber
    extends Reducer
{
    Suber(ScmNumber first)
    { super(first); }

    protected Value combine(Value fst, Value snd)
        throws NumberExpected
    { return fst.toScmNumber().minus(snd.toScmNumber()); }
}

final class Multiplier
    extends Reducer
{
    Multiplier()
    { super(ScmNumber.create(1)); }

    protected Value combine(Value fst, Value snd)
        throws NumberExpected
    { return fst.toScmNumber().times(snd.toScmNumber()); }
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

        ScmNumber curr = arguments.getHead().toScmNumber();
        List         tail = arguments.getTail();

        boolean rising  = true;
        boolean strict  = true;
        boolean falling = true;

        do {
            ScmNumber next = tail.getHead().toScmNumber();
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
    { return ScmBoolean.create(fst.eq(snd)); }

    public final static Value eqv_3F(Value fst, Value snd)
    { return ScmBoolean.create(fst.eqv(snd)); }

    public final static Value equal_3F(Value fst, Value snd)
    { return ScmBoolean.create(fst.equal(snd)); }


    // 6.2 Numbers

    // 6.2.5 Numerical operations
    public final static Value number_3F(Value argument)
    { return ScmBoolean.create(argument.isScmNumber()); }

    public final static Value complex_3F(Value argument)
    { return ScmBoolean.create(argument.isScmNumber()); }

    public final static Value real_3F(Value argument)
    { return ScmBoolean.create(argument.isScmNumber()); }

    public final static Value rational_3F(Value argument)
    { return ScmBoolean.create(argument.isScmNumber()); }

    public final static Value integer_3F(Value argument)
    { return ScmBoolean.create(argument.isScmNumber()); }


    public final static Value exact_3F(Value argument)
    { return ScmBoolean.create(argument.isScmNumber()); }

    public final static Value inexact_3F(Value argument)
    { return ScmBoolean.createFalse(); }


    public final static Value _3C(List arguments) // <
        throws RuntimeError, TypeError
    { return ScmBoolean.create(Order.check(arguments, Order.LT)); }

    public final static Value _3C_3D(List arguments) // <=
        throws RuntimeError, TypeError
    { return ScmBoolean.create(Order.check(arguments, Order.LE)); }

    public final static Value _3D(List arguments) // =
        throws RuntimeError, TypeError
    { return ScmBoolean.create(Order.check(arguments, Order.EQ)); }

    public final static Value _3E_3D(List arguments) // >=
        throws RuntimeError, TypeError
    { return ScmBoolean.create(Order.check(arguments, Order.GE)); }

    public final static Value _3E(List arguments) // >
        throws RuntimeError, TypeError
    { return ScmBoolean.create(Order.check(arguments, Order.GT)); }


    public final static Value zero_3F(Value argument) // zero?
        throws TypeError
    { return ScmBoolean.create(argument.toScmNumber().getInteger() == 0); }


    private final static Adder ADDER = new Adder();

    public final static Value _2B(List arguments) // +
        throws RuntimeError, TypeError
    { return ADDER.reduceLeft(arguments); }


    private final static Arity AT_LEAST_1 = Arity.atLeast(1);

    public final static Value _2D(List arguments) // -
        throws RuntimeError, TypeError
    {
        int len = Function.checkArguments(AT_LEAST_1, arguments);

        ScmNumber first = arguments.getHead().toScmNumber();
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
    { return ScmBoolean.create(!argument.isTrue()); }

    public final static Value boolean_3F(Value argument) // boolean?
    { return ScmBoolean.create(argument.isScmBoolean()); }


    // 6.3.2 Pairs and lists

    public final static Value pair_3F(Value argument) // pair?
    { return ScmBoolean.create(argument.isPair()); }

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
    { return ScmBoolean.create(argument.eq(Empty.create())); }

    public final static Value list_3F(Value argument) // list?
    { return ScmBoolean.create(argument.isList()); }

    public final static Value list(List argument) // list?
    { return argument.toValue(); }

    public final static Value length(Value argument)
        throws ListExpected
    { return ScmNumber.create(argument.toList().getLength()); }

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
    { return ScmBoolean.create(argument.isSymbol()); }

    public final static Value symbol_2D_3Estring(Value argument) // symbol->string
        throws SymbolExpected
    { return ScmString.create(argument.toSymbol()); }

    public final static Value string_2D_3Esymbol(Value argument) // string->symbol
        throws StringExpected
    { return Symbol.create(argument.toScmString()); }


    // 6.3.4 Characters

    public final static Value char_3F(Value argument) // char?
    { return ScmBoolean.create(argument.isScmChar()); }

    public final static Value char_3C_3F(Value fst, Value snd) // char<?
        throws CharExpected
    { return ScmBoolean.create(fst.toScmChar().getJavaChar() < snd.toScmChar().getJavaChar()); }

    public final static Value char_3C_3D_3F(Value fst, Value snd) // char<=?
        throws CharExpected
    { return ScmBoolean.create(fst.toScmChar().getJavaChar() <= snd.toScmChar().getJavaChar()); }

    public final static Value char_3D_3F(Value fst, Value snd) // char=?
        throws CharExpected
    { return ScmBoolean.create(fst.toScmChar().getJavaChar() == snd.toScmChar().getJavaChar()); }

    public final static Value char_3E_3D_3F(Value fst, Value snd) // char>=?
        throws CharExpected
    { return ScmBoolean.create(fst.toScmChar().getJavaChar() >= snd.toScmChar().getJavaChar()); }

    public final static Value char_3E_3F(Value fst, Value snd) // char>?
        throws CharExpected
    { return ScmBoolean.create(fst.toScmChar().getJavaChar() > snd.toScmChar().getJavaChar()); }

    public final static Value char_2D_3Einteger(Value argument)
        throws CharExpected
    { return ScmNumber.create(argument.toScmChar().getJavaChar()); }

    public final static Value integer_2D_3Echar(Value argument)
        throws NumberExpected
    { return ScmChar.create((char)argument.toScmNumber().getInteger()); }

    public final static Value char_2Dupcase(Value argument)
        throws CharExpected
    { return ScmChar.create(Character.toUpperCase(argument.toScmChar().getJavaChar())); }

    public final static Value char_2Ddowncase(Value argument)
        throws CharExpected
    { return ScmChar.create(Character.toLowerCase(argument.toScmChar().getJavaChar())); }


    // 6.3.5 Strings

    public final static Value string_3F(Value argument) // string?
    { return ScmBoolean.create(argument.isScmString()); }


    // 6.3.6 Vectors

    public final static Value vector_3F(Value argument) // vector?
    { return ScmBoolean.create(argument.isScmVector()); }

    public final static Value vector(List arguments) // vector
        throws ListExpected
    { return ScmVector.create(arguments); }

    public final static Value vector_2D_3Elist(Value argument) // vector->list
        throws VectorExpected
    { return argument.toScmVector().getList().toValue(); }

    public final static Value list_2D_3Evector(Value argument) // list->vector
        throws ListExpected
    { return ScmVector.create(argument.toList()); }

    // 6.4 Control features

    public final static Value procedure_3F(Value argument) // procedure?
    { return ScmBoolean.create(argument.isFunction()); }

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
        if (fst.toScmNumber().getInteger() != 5) {
            throw new RuntimeError(fst);
        }

        return DynamicEnvironment.getSchemeReportEnvironment();
    }

    public final static Value null_2Denvironment(Value fst)
        throws RuntimeError, TypeError
    {
        if (fst.toScmNumber().getInteger() != 5) {
            throw new RuntimeError(fst);
        }

        return DynamicEnvironment.getNullEnvironment();
    }


    // 6.6 Input and output

    // 6.6.1 Ports

    public final static Value port_3F(Value argument) // port?
    { return ScmBoolean.create(argument.isPort()); }

    public final static Value input_2Dport_3F(Value argument) // input-port?
        throws PortExpected
    { return ScmBoolean.create(argument instanceof InputPort); }

    public final static Value output_2Dport_3F(Value argument) // output-port?
        throws PortExpected
    { return ScmBoolean.create(argument instanceof OutputPort); }


    public final static Value open_2Dinput_2Dfile(Value argument)
        throws StringExpected, OpenException
    { return InputPort.create(argument.toScmString()); }

    public final static Value open_2Doutput_2Dfile(Value argument)
        throws StringExpected, OpenException
    { return OutputPort.create(argument.toScmString()); }


    public final static Value close_2Dinput_2Dport(Value argument)
        throws PortExpected, CloseException
    { argument.toInputPort().close(); return argument; }
    
    public final static Value close_2Doutput_2Dport(Value argument)
        throws PortExpected, CloseException
    { argument.toOutputPort().close(); return argument; }
    

    // 6.6.2 Input

    public final static Value read(Value fst)
        throws RuntimeError, TypeError
    { return fst.toInputPort().read(); }

    public final static Value read_2Dchar(Value fst)
        throws RuntimeError, TypeError
    { return fst.toInputPort().readScmChar(); }

    public final static Value peek_2Dchar(Value fst)
        throws RuntimeError, TypeError
    { return fst.toInputPort().peekScmChar(); }

    public final static Value eof_2Dobject_3F(Value fst)
    { return ScmBoolean.create(fst.eq(InputPort.EOF_VALUE)); }

    public final static Value char_2Dready(Value fst)
        throws TypeError
    { return ScmBoolean.create(fst.toInputPort().isReady()); }


    // 6.6.3 Output

    public final static Value write(Value fst, Value snd)
        throws RuntimeError, TypeError
    { snd.toOutputPort().write(fst); return snd; }

    public final static Value display(Value fst, Value snd)
        throws RuntimeError, TypeError
    { snd.toOutputPort().display(fst); return snd; }

    public final static Value write_2Dchar(Value fst, Value snd)
        throws RuntimeError, TypeError
    { snd.toOutputPort().writeScmChar(fst.toScmChar()); return snd; }
}

