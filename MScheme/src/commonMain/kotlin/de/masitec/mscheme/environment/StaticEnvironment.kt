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
package de.masitec.mscheme.environment

import de.masitec.mscheme.exceptions.*
import de.masitec.mscheme.syntax.ITranslator
import de.masitec.mscheme.util.Writer
import de.masitec.mscheme.values.IList
import de.masitec.mscheme.values.IOutputable
import de.masitec.mscheme.values.Symbol
import de.masitec.mscheme.values.ValueTraits


class StaticEnvironment internal constructor(
    val parent: StaticEnvironment? = null
) : IOutputable {
    override fun outputOn(destination: Writer, doWrite: Boolean) {
        destination.write("#[static environment]")
    }

    fun toStaticEnvironment(): StaticEnvironment {
        return this
    }

    private val _bindings: MutableMap<Symbol, Any> =
        mutableMapOf()

    // *** instance access ***************************************************

    val level: Int =
        if (parent == null) 0 else (parent.level + 1)
    var size: Int = 0
        private set

    // *** constructors ******************************************************

    internal constructor(
        parent: StaticEnvironment?, symbols: IList
    ) : this(parent) {
        var tail = symbols

        while (!tail.isEmpty) {
            define(ValueTraits.toSymbol(tail.head))
            tail = tail.tail
        }
    }

    internal constructor(
        parent: StaticEnvironment?, symbol: Symbol
    ) : this(parent) {
        define(symbol)
    }

    fun createChild(): StaticEnvironment =
        StaticEnvironment(this)

    fun createChild(symbols: IList): StaticEnvironment =
        StaticEnvironment(this, symbols)

    fun createChild(symbol: Symbol): StaticEnvironment =
        StaticEnvironment(this, symbol)

    // *** instance access ***************************************************
    fun define(symbol: Symbol): Reference {
        try {
            val existingReference = _bindings[symbol] as Reference?

            // if ref is != null
            // the symbol is already bound in the
            // current frame. This define is in fact
            // a lookup.
            if (existingReference != null) {
                if (level == 0) {
                    return existingReference
                }
            } else {
                val newReference =
                    Reference.create(symbol, level, size)

                _bindings[symbol] = newReference
                size++

                return newReference
            }
        } catch (e: ClassCastException) {
        }

        throw AlreadyBound(symbol)
    }


    fun defineSyntax(symbol: Symbol, value: ITranslator) {
        val binding = _bindings[symbol]

        if (binding != null && binding !is ITranslator) {
            throw AlreadyBound(symbol)
        }

        _bindings[symbol] = value
    }


    private fun lookupNoThrow(symbol: Symbol): Any? {
        var current: StaticEnvironment? = this

        while (current != null) {
            val result = current._bindings[symbol]

            if (result != null) {
                return result
            }

            current = current.parent
        }

        return null
    }

    private fun delayedLookup(symbol: Symbol, restricted: Boolean): Any {
        val result = lookupNoThrow(symbol)

        return if ((result == null) || (result is Reference))
            Reference.create(symbol, this, restricted)
        else
            result
    }

    private fun lookup(symbol: Symbol): Any =
        lookupNoThrow(symbol) ?: throw SymbolNotFoundException(symbol)

    fun getSyntaxFor(symbol: Symbol): ITranslator? =
        lookup(symbol) as? ITranslator

    fun getDelayedReferenceFor(symbol: Symbol, restricted: Boolean): Reference {
        val o = delayedLookup(symbol, restricted)

        if (o is Reference) {
            return o
        } else {
            throw UnexpectedSyntax(symbol)
        }
    }

    fun getReferenceFor(symbol: Symbol, restricted: Boolean): Reference {
        val o = lookup(symbol)

        if (o is Reference) {
            if (restricted
                && (o.level == this.level)
                && (this.level > 0) // and again: global is special
            ) {
                throw CompileError(symbol, "may not be used here")
            }

            return o
        } else {
            throw UnexpectedSyntax(symbol)
        }
    }

    fun getReferenceFor(symbol: Symbol): Reference =
        getReferenceFor(symbol, false)

    fun isBound(symbol: Symbol): Boolean =
        lookupNoThrow(symbol) != null

    // ***********************************************************************

    companion object {
        fun create(): StaticEnvironment =
            StaticEnvironment()
    }
}
