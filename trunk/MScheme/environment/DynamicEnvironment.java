package MScheme.environment;


import java.util.Vector;

import MScheme.util.Arity;
import MScheme.values.*;
import MScheme.code.Code;
import MScheme.code.Syntax;
import MScheme.code.SyntaxFactory;
import MScheme.exceptions.*;


public class DynamicEnvironment
{
    // *******************************************************************

    private StaticEnvironment _bindings;
    private Vector[]          _data;

    // *******************************************************************

    private DynamicEnvironment(
        StaticEnvironment bindings,
        Vector[]          data
    )
    { _bindings = bindings; _data = data; }

    DynamicEnvironment()
    { this(new StaticEnvironment(), (DynamicEnvironment)null); }

    DynamicEnvironment(
        StaticEnvironment bindings,
        DynamicEnvironment parent
    )
    {
        int level = bindings.getLevel();
    
        _bindings = bindings;
        _data     = new Vector[level + 1];

        if (level > 0) {
            if (parent._bindings != bindings.getParent()) {
                throw new RuntimeException(
                    "consistency failure: StaticEnvironment parent"
                );
            }
            
            System.arraycopy(
                parent._data, 0,
                _data, 0,
                level
            );
        }

        Vector newData = new Vector();
        newData.setSize(bindings.getSize());
        _data[level] = newData;
    }

    DynamicEnvironment(
        StaticEnvironment  bindings,
        DynamicEnvironment parent,
        Arity              arity,
        List               values
    ) throws ListExpectedException
    {
        this(bindings, parent);

        Vector data = _data[_bindings.getLevel()];
        List   tail = values;

        for (int i = 0; i < arity.getMin(); i++)
        {
            data.setElementAt(
                tail.getHead(),
                i
            );
            
            tail = tail.getTail();
        }

        if (arity.allowMore()) {
            data.setElementAt(
                tail,
                arity.getMin()
            );
        }
    }


    public static DynamicEnvironment getEmpty()
    { return new DynamicEnvironment(); }

    public static DynamicEnvironment getNullEnvironment()
    {
        DynamicEnvironment result = getEmpty();

        try {
            StaticEnvironment staticBindings = result.getStatic();

            staticBindings.defineSyntax(
                ValueFactory.createSymbol("quote"),
                SyntaxFactory.getQuoteToken()
            );
            staticBindings.defineSyntax(
                ValueFactory.createSymbol("cond"),
                SyntaxFactory.getCondToken()
            );
            staticBindings.defineSyntax(
                ValueFactory.createSymbol("if"),
                SyntaxFactory.getIfToken()
            );
            staticBindings.defineSyntax(
                ValueFactory.createSymbol("begin"),
                SyntaxFactory.getBeginToken()
            );
            staticBindings.defineSyntax(
                ValueFactory.createSymbol("lambda"),
                SyntaxFactory.getLambdaToken()
            );
            staticBindings.defineSyntax(
                ValueFactory.createSymbol("let"),
                SyntaxFactory.getLetToken()
            );
            staticBindings.defineSyntax(
                ValueFactory.createSymbol("define"),
                SyntaxFactory.getDefineToken()
            );
            staticBindings.defineSyntax(
                ValueFactory.createSymbol("set!"),
                SyntaxFactory.getSetToken()
            );
        }
        catch (SyntaxException e) {
            throw new RuntimeException(
                "unexpected SyntaxException in getNullEnvironment()"
            );
        }

        return result;
    }

    private final static String[][] dynamicBindings = {
        // 6. Standard procedures

        // 6.1 Equivalence predicates
        {"eqv?",    "Eqv"},
        {"eq?",     "Eq"},
        {"equal?",  "Equal"},

        // 6.2 Numbers

        // 6.2.5 Numerical operations
        {"number?",     "IsNumber"},
        {"complex?",    "IsNumber"},
        {"real?",       "IsNumber"},
        {"rational?",   "IsNumber"},
        {"integer?",    "IsNumber"},

        {"exact?",      "IsNumber"},
//      {"inexact?",    "IsNumber"},

//      {"=",   "NumberEQ"},
        {"<",   "NumberLT"},
        {">",   "NumberGT"},
        {"<=",  "NumberLE"},
        {">=",  "NumberGE"},

        {"+",   "Plus"},
        {"*",   "Times"},

        {"-",   "Minus"},
//      {"/",   "Slash"},

//      {"quotient",  },
//      {"remainder", },
//      {"modulo",    },

//      {"numerator",    },
//      {"denominator",  },

//      {"floor",        },
//      {"ceiling",      },
//      {"truncate",     },
//      {"round",        },

//      {"exp",          },
//      {"log",          },
//      {"sin",          },
//      {"cos",          },
//      {"tan",          },
//      {"asin",         },
//      {"acos",         },
//      {"atan",         },

//      {"sqrt",         },

//      {"expt",         },

//      {"make-rectangular", },
//      {"make-polar",   },
//      {"real-part",    },
//      {"imag-part",    },
//      {"magnitude",    },
//      {"angle",        },

//      {"exact->inexact", },
//      {"inexact->exact", },

        // 6.2.6 Numerical input and output
//      {"number->string", },
//      {"string->number", },


        // 6.3 Other data types
        
        // 6.3.1 Booleans
//      {"not",                    },
        {"boolean?",    "IsBoolean"},

        // 6.3.2 Pairs and lists
        {"pair?",       "IsPair"},
        {"cons",        "Cons"},
        {"car",         "Car"},
        {"cdr",         "Cdr"},
        {"set-car!",    "SetCar"},
        {"set-cdr!",    "SetCdr"},

        {"null?",       "IsEmpty"},
        {"list?",       "IsList"},
        {"list",        "List"},
        {"length",      "Length"},
        {"append",      "Append"},
        {"reverse",     "Reverse"},

        {"memq",        "Memq"},
        {"memv",        "Memv"},
        {"member",      "Member"},

        {"assq",        "Assq"},
        {"assv",        "Assv"},
        {"assoc",       "Assoc"},

        // 6.3.3 Symbols
        {"symbol?",        "IsSymbol"},
        {"symbol->string", "SymbolToString"},
        {"string->symbol", "StringToSymbol"},

        // 6.3.4 Characters
        {"char?",         "IsChar"},

        {"char=?",        "CharEQ"},
        {"char<?",        "CharLT"},
        {"char>?",        "CharGT"},
        {"char<=?",       "CharLE"},
        {"char>=?",       "CharGE"},

        {"char->integer", "CharToInteger"},
        {"integer->char", "IntegerToChar"},

        {"char-upcase",   "CharUpcase"},
        {"char-downcase", "CharDowncase"},

        // 6.3.5 Strings
        {"string?",       "IsString"},
        {"make-string",   "MakeString"},
        {"string-length", "StringLength"},
        {"string-ref",    "StringRef"},
        {"string-set!",   "StringSet"},

        // 6.3.6 Vectors
        {"vector?",     "IsVector"},
        {"make-vector",   "MakeVector"},
        {"vector-length", "VectorLength"},
        {"vector-ref",    "VectorRef"},
        {"vector-set!",   "VectorSet"},

        // 6.4 Control features
        {"procedure?",    "IsProcedure"},
        {"apply",         "Apply"},
        {"force",         "Force"},
        {"call-with-current-continuation", "CallCC"},
        {"dynamic-wind",  "DynamicWind"},

        // 6.5 Eval

        // 6.6 Input and output

        // 6.6.1 Ports
        {"port?",        "IsPort"},
        {"input-port?",  "IsInputPort"},
        {"output-port?", "IsOutputPort"},

        {"open-input-file",   "OpenInput"},
        {"open-output-file",  "OpenOutput"},

        {"close-input-port",  "CloseInput"},
        {"close-output-port", "CloseOutput"},

        // 6.6.2 Input
        {"read",         "Read"},
        {"read-char",    "ReadChar"},
        {"peek-char",    "PeekChar"},
        {"eof-object",   "IsEof"},
        {"char-ready",   "IsCharReady"},

        // 6.6.3 Output
        {"write",        "Write"},
        {"display",      "Display"},
        {"write-char",   "WriteChar"}
    };

    public static DynamicEnvironment getSchemeReportEnvironment()
    {
        DynamicEnvironment result = getNullEnvironment();

        for (int i = 0; i < dynamicBindings.length; i++) {
            try {
                result.define(
                    ValueFactory.createSymbol  (dynamicBindings[i][0]),
                    ValueFactory.createFunction(dynamicBindings[i][1])
                );
            }
            catch (SyntaxException e) {
                throw new RuntimeException(
                    "unexpected SyntaxException"
                );
            }
            catch (FunctionNotFoundException e) {
                System.err.println("not found: " + dynamicBindings[i][1]);
//              throw new RuntimeException(
//                  "unexpected FunctionNotFoundException"
//              );
            }
        }

        return result;
    }



    public StaticEnvironment getStatic()
    { return _bindings; }

    public DynamicEnvironment getParent()
    { return new DynamicEnvironment(_bindings.getParent(), _data); }

    public DynamicEnvironment newChild()
    { return newChild(_bindings.newChild()); }

    public DynamicEnvironment newChild(
        StaticEnvironment newFrame
    )
    { return new DynamicEnvironment(newFrame, this); }
    
    public DynamicEnvironment newChild(
        StaticEnvironment newFrame,
        Arity             arity,
        List              values
    ) throws ListExpectedException
    { return new DynamicEnvironment(newFrame, this, arity, values); }

    // *** Envrionment access ************************************************

    // *** code access (compiletime) ***
        
    public Reference define(Symbol key, Value value)
        throws SyntaxException
    {
        Reference newReference = _bindings.define(key);
        assign(newReference, value);
        return newReference;
    }

    // *** value access (runtime) ***

    public void assign(Reference key, Value value)
    {
        Vector data  = _data[key.getLevel()];
        int    index = key.getIndex();
    
        try {
            data.setElementAt(value, index);
        }
        catch (ArrayIndexOutOfBoundsException e) {
            data.setSize(index + 1);
            data.setElementAt(value, index);
        }
    }

    public void assign(Symbol key, Value value)
        throws SymbolNotFoundException, SyntaxException
    { assign(_bindings.getReferenceFor(key), value); }


    public Value lookup(Reference ref)
        throws UninitializedSymbolException
    {
        Value result;
        
        try {
            result = (Value)_data[
                ref.getLevel()
            ].get(
                ref.getIndex()
            );
        }
        catch (ArrayIndexOutOfBoundsException e) {
            result = null;
        }
    
        if (result == null) {
            throw new UninitializedSymbolException(ref.getSymbol());
        }
        
        return result;
    }

    public Value lookup(Symbol key)
        throws SymbolNotFoundException,
               SyntaxException,
               UninitializedSymbolException
    { return lookup(_bindings.getReferenceFor(key)); }

    // ***********************************************************************
}

