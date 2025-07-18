/* Maps symbols to References.
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
package mscheme.environment

import mscheme.exceptions.*
import mscheme.syntax.ITranslator
import mscheme.values.IList
import mscheme.values.ValueTraits
import java.io.IOException
import java.io.Writer
import java.util.*

class StaticEnvironment @JvmOverloads internal constructor(
    val parent: StaticEnvironment? = null
) {
    @Throws(IOException::class)
    fun writeOn(destination: Writer) {
        destination.write("#[static environment]")
    }

    fun toStaticEnvironment(): StaticEnvironment {
        return this
    }

    private val _bindings: MutableMap<String, Any> =
        Hashtable()

    // *** instance access ***************************************************

    val level: Int =
        if (parent == null) 0 else (parent.level + 1)
    var size: Int = 0
        private set
    private var _state: State =
        State.OPEN

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
        parent: StaticEnvironment?, symbol: String
    ) : this(parent) {
        define(symbol)
    }

    fun createChild(): StaticEnvironment =
        StaticEnvironment(this)

    @Throws(CompileError::class, TypeError::class)
    fun createChild(symbols: IList): StaticEnvironment =
        StaticEnvironment(this, symbols)

    @Throws(CompileError::class)
    fun createChild(symbol: String): StaticEnvironment =
        StaticEnvironment(this, symbol)

    // *** instance access ***************************************************
    @Throws(CompileError::class)
    fun setStateOpen(v: Any?) {
        when (_state) {
            State.OPEN -> throw CompileError(
                v,
                "no nested definitions"
            )

            State.DEF_BODY -> _state = State.OPEN
            State.CLOSED -> throw CompileError(
                v,
                "environment already closed (1)"
            )
        }
    }

    @Throws(CompileError::class)
    fun setStateDefinitionBody(v: Any?) {
        when (_state) {
            State.OPEN -> _state = State.DEF_BODY
            State.DEF_BODY -> throw CompileError(
                v,
                "no nested definitions"
            )

            State.CLOSED -> throw CompileError(
                v,
                "environment already closed (2)"
            )
        }
    }

    fun setStateClosed() {
        if ((_state == State.OPEN) && (this.level > 0)) {
            _state = State.CLOSED
        }
    }

    @Throws(CompileError::class)
    fun define(symbol: String): Reference {
        if (_state != State.OPEN) {
            throw CompileError(
                symbol,
                "environment already closed (3)"
            )
        }

        try {
            val key = symbol
            var ref = _bindings[key] as Reference?

            // if ref is != null
            // the symbol is already bound in the
            // current frame. This define is in fact
            // a lookup.
            if (ref == null) {
                ref = Reference.create(
                    symbol,
                    this.level,
                    this.size
                )

                _bindings[key] = ref
                this.size++

                return ref
            } else if (this.level == 0) {
                return ref
            }
        } catch (e: ClassCastException) {
        }

        throw AlreadyBound(symbol)
    }


    @Throws(AlreadyBound::class)
    fun defineSyntax(symbol: String, value: ITranslator) {
        val key = symbol

        run {
            val o = _bindings[key]
            if ((o != null) && o !is ITranslator) {
                throw AlreadyBound(symbol)
            }
        }

        _bindings[key] = value
    }


    private fun lookupNoThrow(key: String): Any? {
        var current: StaticEnvironment? = this

        while (current != null) {
            val result = current._bindings[key]

            if (result != null) {
                return result
            }

            current = current.parent
        }

        return null
    }

    private fun delayedLookup(key: String): Any {
        val result = lookupNoThrow(key)

        return if ((result == null) || (result is Reference))
            Reference.create(key, this, _state == State.DEF_BODY)
        else
            result
    }

    @Throws(SymbolNotFoundException::class)
    private fun lookup(key: String): Any =
        lookupNoThrow(key) ?: throw SymbolNotFoundException(key)

    @Throws(SymbolNotFoundException::class)
    fun getSyntaxFor(key: String): ITranslator? =
        lookup(key) as? ITranslator

    @Throws(UnexpectedSyntax::class)
    fun getDelayedReferenceFor(key: String): Reference {
        val o = delayedLookup(key)

        if (o is Reference) {
            return o
        } else {
            throw UnexpectedSyntax(key)
        }
    }

    @Throws(CompileError::class)
    fun getReferenceFor(key: String, restricted: Boolean): Reference {
        val o = lookup(key)

        if (o is Reference) {
            if (restricted
                && (o.level == this.level)
                && (this.level > 0) // and again: global is special
            ) {
                throw CompileError(key, "may not be used here")
            }

            return o
        } else {
            throw UnexpectedSyntax(key)
        }
    }

    @Throws(CompileError::class)
    fun getReferenceFor(key: String): Reference =
        getReferenceFor(key, false)

    fun isBound(key: String): Boolean =
        lookupNoThrow(key) != null

    // ***********************************************************************

    companion object {
        @JvmStatic
        fun create(): StaticEnvironment =
            StaticEnvironment()
    }

    enum class State {
        OPEN, DEF_BODY, CLOSED
    }
}
