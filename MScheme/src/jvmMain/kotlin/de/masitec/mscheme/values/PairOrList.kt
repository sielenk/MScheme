/*
 * Copyright (C) 2025  Marvin H. Sielenkemper
 *
 * This file is part of MScheme.
 *
 * MScheme is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License,
 * or (at your option) any later version.
 *
 * MScheme is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with MScheme; see the file COPYING. If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA  02111-1307, USA.
 */
package de.masitec.mscheme.values

import de.masitec.mscheme.compiler.Compiler
import de.masitec.mscheme.environment.StaticEnvironment
import de.masitec.mscheme.exceptions.ListExpected
import de.masitec.mscheme.exceptions.PairExpected
import de.masitec.mscheme.syntax.ITranslator
import de.masitec.mscheme.syntax.ProcedureCall
import de.masitec.mscheme.util.Writer
import de.masitec.mscheme.util.writeToString


internal class ConstPairOrList(
    first: Any?,
    second: Any?
) : PairOrList() {
    override val first: Any? =
        ValueTraits.getConst(first)
    override val second: Any? =
        ValueTraits.getConst(second)
}

internal class MutablePairOrList(
    override var first: Any?,
    override var second: Any?
) : PairOrList(), IMutablePair {
    override fun getConst(): Any =
        ConstPairOrList(this.first, this.second)
}

internal class ListIterator(private var _tortoise: Any?) : Iterator<Any?> {
    private var _hare: Any?

    init {
        _hare =
            if (_tortoise is IPair) (_tortoise as IPair).second else _tortoise
    }

    override fun hasNext(): Boolean =
        _hare !== _tortoise

    val isCyclic: Boolean
        get() = _tortoise is IPair

    val isValid: Boolean
        get() = _tortoise is Empty

    override fun next(): Any? {
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
    override fun outputOn(destination: Writer, doWrite: Boolean) {
        destination.write('('.code)

        val enumerator = ListIterator(this)
        var first = true
        while (enumerator.hasNext()) {
            if (!first) {
                destination.write(' '.code)
            }

            ValueTraits.output(destination, doWrite, enumerator.next())
            first = false
        }

        if (!enumerator.isValid) {
            if (enumerator.isCyclic) {
                destination.write(" . [ cyclic ]")
            } else {
                destination.write(" . ")
                ValueTraits.output(
                    destination, doWrite, enumerator.next()
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

    fun getTranslator(compilationEnv: StaticEnvironment?): ITranslator =
        ProcedureCall.create(this)

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

    override fun validate(): IList =
        if (isValid)
            this
        else
            throw ListExpected(this)

    override val isEmpty: Boolean
        get() = false

    override fun getCopy(): IList {
        val empty: Any = ListFactory.create()

        val enumerator = ListIterator(this)
        val result = MutablePairOrList(
            enumerator.next(), empty
        )
        var current = result
        while (enumerator.hasNext()) {
            val next = MutablePairOrList(
                enumerator.next(), empty
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

    override fun getCompiledArray(compilationEnv: StaticEnvironment): Array<Any?> =
        getCompiledArray(compilationEnv, 0)

    override fun getCompiledArray(
        compilationEnv: StaticEnvironment,
        index: Int
    ): Array<Any?> {
        val compiledHead = Compiler(compilationEnv)
            .getForceable(head)
        val result = tail.getCompiledArray(compilationEnv, index + 1)

        result[index] = compiledHead

        return result
    }

    override fun getArray(): Array<Any?> =
        getArray(0)

    override fun getArray(index: Int): Array<Any?> {
        val result = tail.getArray(index + 1)

        result[index] = head

        return result
    }

    override fun toString(): String =
        writeToString {
            outputOn(it, true)
        }

    companion object {
        fun prepend(head: Any?, tail: IList): IList {
            return MutablePairOrList(head, tail)
        }

        fun prependConst(head: Any?, tail: IList): IList {
            return ConstPairOrList(head, tail)
        }

        fun create(first: Any?, second: Any?): IMutablePair {
            return MutablePairOrList(first, second)
        }

        fun createConst(first: Any?, second: Any?): IPair {
            return ConstPairOrList(first, second)
        }
    }
}
