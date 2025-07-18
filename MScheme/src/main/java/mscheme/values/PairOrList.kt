/*
 * The implementation of Scheme's pairs. Copyright (C) 2001 Marvin H.
 * Sielenkemper
 *
 * This file is part of MScheme.
 *
 * MScheme is free software; you can redistribute it and/or modify it under the
 * terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 2 of the License, or (at your option) any later
 * version.
 *
 * MScheme is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
 * A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * MScheme; see the file COPYING. If not, write to the Free Software Foundation,
 * Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */
package mscheme.values

import mscheme.compiler.Compiler
import mscheme.environment.StaticEnvironment
import mscheme.exceptions.ListExpected
import mscheme.exceptions.PairExpected
import mscheme.exceptions.SchemeException
import mscheme.syntax.ITranslator
import mscheme.syntax.ProcedureCall
import mscheme.values.ValueTraits.getConst
import mscheme.values.ValueTraits.output
import java.io.IOException
import java.io.StringWriter
import java.io.Writer
import java.util.*


internal class ConstPairOrList(
    first: Any?,
    second: Any?
) : PairOrList() {
    override val first: Any? =
        getConst(first)
    override val second: Any? =
        getConst(second)
}

internal class MutablePairOrList(
    override var first: Any?,
    override var second: Any?
) : PairOrList(), IMutablePair {
    override fun getConst(): Any =
        ConstPairOrList(this.first, this.second)
}

internal class ListEnumerator(private var _tortoise: Any?) : Enumeration<Any?> {
    private var _hare: Any?

    init {
        _hare = if (_tortoise is IPair) (_tortoise as IPair).second else _tortoise
    }

    override fun hasMoreElements(): Boolean =
        _hare !== _tortoise

    val isCyclic: Boolean
        get() = _tortoise is IPair

    val isValid: Boolean
        get() = _tortoise is Empty

    override fun nextElement(): Any? {
        val tortoise = _tortoise

        if (tortoise is IPair) {
            if (_hare is IPair) {
                _hare = (_hare as IPair).second
            }

            if (_hare is IPair) {
                _hare = (_hare as IPair).second
            }

            _tortoise = tortoise.second
            return tortoise.first
        } else {
            return tortoise
        }
    }
}

abstract class PairOrList protected constructor(
) : IPair, IList, ICompileable, IComparable, IOutputable {
    // implementation of Outputable
    @Throws(IOException::class)
    override fun outputOn(destination: Writer, doWrite: Boolean) {
        destination.write('('.code)

        val enumerator = ListEnumerator(this)
        var first = true
        while (enumerator.hasMoreElements()) {
            if (!first) {
                destination.write(' '.code)
            }

            output(destination, doWrite, enumerator.nextElement())
            first = false
        }

        if (!enumerator.isValid) {
            if (enumerator.isCyclic) {
                destination.write(" . [ cyclic ]")
            } else {
                destination.write(" . ")
                output(
                    destination, doWrite, enumerator
                        .nextElement()
                )
            }
        }

        destination.write(')'.code)
    }

    override fun eq(other: Any?): Boolean =
        this === other

    override fun eqv(other: Any?): Boolean =
        this === other

    override fun equals(other: Any?): Boolean =
        other is PairOrList
                && ValueTraits.equal(first, other.first)
                && ValueTraits.equal(second, other.second)

    @Throws(SchemeException::class)
    fun getTranslator(compilationEnv: StaticEnvironment?): ITranslator =
        ProcedureCall.create(this)

    @Throws(SchemeException::class, InterruptedException::class)
    override fun getForceable(compilationEnv: StaticEnvironment): Any? {
        val list = validate()

        return Compiler(compilationEnv)
            .getTranslator(list.head)
            .translate(compilationEnv, list.tail)
    }

    override val isValid: Boolean
        // implementation of List
        get() {
            var hare = second

            if (hare is Empty) {
                return true
            }

            var tortoise = hare
            do {
                if (hare is IPair) {
                    hare = hare.second
                } else {
                    return (hare is Empty)
                }

                if (hare is IPair) {
                    hare = hare.second
                } else {
                    return (hare is Empty)
                }

                tortoise = (tortoise as IPair).second
            } while (hare !== tortoise)

            return false
        }

    @Throws(ListExpected::class)
    override fun validate(): IList =
        if (isValid)
            this
        else
            throw ListExpected(this)

    override val isEmpty: Boolean
        get() = false

    override fun getCopy(): IList {
        val empty: Any = ListFactory.create()

        val enumerator = ListEnumerator(this)
        val result = MutablePairOrList(
            enumerator
                .nextElement(), empty
        )
        var current = result
        while (enumerator.hasMoreElements()) {
            val next = MutablePairOrList(
                enumerator
                    .nextElement(), empty
            )
            current.second = next
            current = next
        }

        return result
    }

    override val head: Any?
        get() = first

    override val tail: IList
        get() = second as IList

    override val length: Int
        get() {
            try {
                var result = 1

                var tail = tail
                while (!tail.isEmpty) {
                    ++result
                    tail = tail.tail
                }

                return result
            } catch (e: PairExpected) {
                throw RuntimeException("unexpected PairExpected")
            }
        }

    override fun getReversed(): IList {
        try {
            var result = ListFactory.create()

            var rest: IList = this
            while (!rest.isEmpty) {
                result = ListFactory.prepend(rest.head, result)
                rest = rest.tail
            }

            return result
        } catch (e: PairExpected) {
            throw RuntimeException("unexpected PairExpected")
        }
    }

    @Throws(SchemeException::class, InterruptedException::class)
    override fun getCompiledArray(compilationEnv: StaticEnvironment): Array<Any?> =
        getCompiledArray(compilationEnv, 0)

    @Throws(SchemeException::class, InterruptedException::class)
    override fun getCompiledArray(compilationEnv: StaticEnvironment, index: Int): Array<Any?> {
        val compiledHead = Compiler(compilationEnv)
            .getForceable(head)
        val result = tail.getCompiledArray(compilationEnv, index + 1)

        result!![index] = compiledHead

        return result
    }

    override fun getArray(): Array<Any?> =
        getArray(0)

    override fun getArray(index: Int): Array<Any?> {
        val result = tail.getArray(index + 1)

        result[index] = head

        return result
    }

    override fun toString(): String {
        val out = StringWriter()
        try {
            outputOn(out, true)
        } catch (e: IOException) {
            e.printStackTrace()
        }
        return out.toString()
    }

    companion object {
        @JvmStatic
        fun prepend(head: Any?, tail: IList): IList {
            return MutablePairOrList(head, tail)
        }

        @JvmStatic
        fun prependConst(head: Any?, tail: IList): IList {
            return ConstPairOrList(head, tail)
        }

        @JvmStatic
        fun create(first: Any?, second: Any?): IMutablePair {
            return MutablePairOrList(first, second)
        }

        @JvmStatic
        fun createConst(first: Any?, second: Any?): IPair {
            return ConstPairOrList(first, second)
        }
    }
}