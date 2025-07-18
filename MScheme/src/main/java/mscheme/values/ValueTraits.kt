/*
 * Created on 02.01.2004
 *
 */
package mscheme.values

import mscheme.environment.Environment
import mscheme.environment.StaticEnvironment
import mscheme.exceptions.*
import mscheme.machine.Registers
import mscheme.util.Arity
import java.io.IOException
import java.io.Writer
import java.lang.reflect.Field
import java.lang.reflect.InvocationTargetException
import java.lang.reflect.Method
import java.lang.reflect.Modifier

/**
 * @author sielenk
 */
object ValueTraits {
    const val TRUE: Boolean = true

    const val FALSE: Boolean = false

    @JvmStatic
    fun isTrue(o: Any?): Boolean =
        o != false

    @JvmStatic
    fun isEmpty(obj: Any?): Boolean =
        obj === Empty

    @JvmStatic
    @Throws(SchemeException::class, InterruptedException::class)
    fun apply(state: Registers, function: Any?, arguments: IList): Any? {
        when (function) {
            is Method -> {
                val parameterTypes = function.parameterTypes
                val methodExpectsIList = ((parameterTypes.size == 1) &&
                        (parameterTypes[0] == IList::class.java))

                return try {
                    if (arguments.isEmpty) {
                        if (methodExpectsIList) {
                            function.invoke(null, arguments)
                        } else {
                            function.invoke(null)
                        }
                    } else if (Modifier.isStatic(function.modifiers)) {
                        if (methodExpectsIList) {
                            function.invoke(null, arguments)
                        } else {
                            function.invoke(null, *arguments.getArray())
                        }
                    } else {
                        if (methodExpectsIList) {
                            function.invoke(arguments.head, arguments.tail)
                        } else {
                            function.invoke(
                                arguments.head,
                                *arguments.tail.getArray()
                            )
                        }
                    }
                } catch (e1: IllegalArgumentException) {
                    throw RuntimeError(function, e1.toString())
                } catch (e1: IllegalAccessException) {
                    throw RuntimeError(function, e1.toString())
                } catch (e1: InvocationTargetException) {
                    throw RuntimeError(function, e1.toString())
                }
            }

            is Field -> {
                try {
                    if (Modifier.isStatic(function.modifiers)) {
                        if (!arguments.isEmpty) {
                            throw RuntimeArityError(arguments, Arity.exactly(0))
                        }

                        return function.get(null)
                    } else {
                        if (!arguments.tail.isEmpty) {
                            throw RuntimeArityError(arguments, Arity.exactly(1))
                        }

                        return function.get(arguments.head)
                    }
                } catch (e: IllegalArgumentException) {
                    throw RuntimeError(function, e.toString())
                } catch (e: IllegalAccessException) {
                    throw RuntimeError(function, e.toString())
                }
            }

            is Function -> {
                return function.call(state, arguments)
            }

            else -> {
                throw FunctionExpected(function)
            }
        }
    }

    @JvmStatic
    fun eq(fst: Any?, snd: Any?): Boolean =
        if (fst is IComparable)
            fst.eq(snd)
        else
            fst === snd

    @JvmStatic
    fun eqv(fst: Any?, snd: Any?): Boolean =
        if (fst is Char)
            fst == snd
        else if (fst is IComparable)
            fst.eqv(snd)
        else
            fst === snd


    @JvmStatic
    fun equal(fst: Any?, snd: Any?): Boolean =
        fst == snd

    @JvmStatic
    fun isList(o: Any?): Boolean =
        o is IList && o.isValid

    @JvmStatic
    @Throws(ListExpected::class)
    fun toList(o: Any?): IList =
        if (o is IList)
            o.validate()
        else
            throw ListExpected(o)

    @JvmStatic
    fun isPair(o: Any?): Boolean =
        o is IPair

    @JvmStatic
    @Throws(PairExpected::class)
    fun toConstPair(o: Any?): IPair {
        if (o is IPair) {
            return o
        } else {
            throw PairExpected(o)
        }
    }

    @JvmStatic
    @Throws(PairExpected::class, ImmutableException::class)
    fun toMutablePair(o: Any?): IMutablePair =
        when (o) {
            is IMutablePair ->
                o

            !is IPair ->
                throw PairExpected(o)

            else ->
                throw ImmutableException(o)
        }


    @JvmStatic
    @Throws(InputPortExpected::class)
    fun toInputPort(o: Any?): InputPort =
        o as? InputPort ?: throw InputPortExpected(o)

    @JvmStatic
    @Throws(SymbolExpected::class)
    fun toSymbol(o: Any?): String =
        o as? String ?: throw SymbolExpected(o)

    private var _index = 0

    @JvmStatic
    fun createUniqueSymbol(): String {
        return "#[" + _index++ + "]"
    }

    @JvmStatic
    fun toScmBoolean(b: Boolean): Boolean =
        b

    fun toScmBoolean(o: Any?): Boolean =
        isTrue(o)

    @JvmStatic
    @Throws(NumberExpected::class)
    fun toScmNumber(o: Any?): ScmNumber =
        o as? ScmNumber ?: throw NumberExpected(o)

    @JvmStatic
    fun toScmNumber(i: Int): Any =
        ScmNumber.create(i)

    @JvmStatic
    @Throws(CharExpected::class)
    fun toScmChar(o: Any?): Char =
        o as? Char ?: throw CharExpected(o)

    @JvmStatic
    fun toScmChar(c: Char): Char =
        c

    @JvmStatic
    @Throws(StringExpected::class)
    fun toScmString(o: Any?): ScmString =
        o as? ScmString ?: throw StringExpected(o)

    @JvmStatic
    @Throws(VectorExpected::class)
    fun toScmVector(o: Any?): ScmVector =
        o as? ScmVector ?: throw VectorExpected(o)

    @JvmStatic
    @Throws(OutputPortExpected::class)
    fun toOutputPort(o: Any?): OutputPort =
        o as? OutputPort ?: throw OutputPortExpected(o)

    @JvmStatic
    @Throws(EnvironmentExpected::class)
    fun toEnvironment(o: Any?): Environment =
        o as? Environment ?: throw EnvironmentExpected(o)

    @JvmStatic
    @Throws(EnvironmentExpected::class)
    fun toStaticEnvironment(o: Any?): StaticEnvironment =
        o as? StaticEnvironment ?: throw EnvironmentExpected(o)

    @JvmStatic
    fun isScmBoolean(o: Any?): Boolean =
        o is Boolean

    @JvmStatic
    fun isSymbol(o: Any?): Boolean =
        o is String

    @JvmStatic
    fun isScmNumber(o: Any?): Boolean =
        o is ScmNumber

    @JvmStatic
    fun isScmChar(o: Any?): Boolean =
        o is Char

    @JvmStatic
    fun isScmString(o: Any?): Boolean =
        o is ScmString

    @JvmStatic
    fun isScmVector(o: Any?): Boolean =
        o is ScmVector

    @JvmStatic
    fun isPort(o: Any?): Boolean =
        o is Port

    @JvmStatic
    fun isFunction(o: Any?): Boolean =
        o is Function || o is Method || o is Field

    @JvmStatic
    @Throws(IOException::class)
    fun output(destination: Writer, doWrite: Boolean, o: Any?) {
        if (o is Char) {
            if (doWrite) {
                destination.write("#\\")

                when (o) {
                    ' ' -> destination.write("space")
                    '\n' -> destination.write("newline")
                    else -> destination.write(o.code)
                }
            } else {
                destination.write(o.code)
            }
        } else if (isScmBoolean(o)) {
            destination.write(if (isTrue(o)) "#t" else "#f")
        } else if (o is IOutputable) {
            o.outputOn(destination, doWrite)
        } else {
            if (doWrite) {
                destination.write("#[$o]")
            } else {
                destination.write(o.toString())
            }
        }
    }

    @JvmStatic
    @Throws(IOException::class)
    fun display(destination: Writer, o: Any?) {
        output(destination, false, o)
    }

    @JvmStatic
    @Throws(IOException::class)
    fun write(destination: Writer, o: Any?) {
        output(destination, true, o)
    }

    @JvmStatic
    fun getConst(o: Any?): Any? =
        if (o is IMutable)
            o.getConst()
        else
            o
}