SRC =	values/Function.java	\
	values/ScmChar.java	\
	values/Compound.java	\
	values/ScmString.java	\
	values/Symbol.java	\
	values/ScmNumber.java	\
	values/List.java	\
	values/ValueFactory.java	\
	values/OutputPort.java	\
	values/Pair.java	\
	values/InputPort.java	\
	values/ScmVector.java	\
	values/Empty.java	\
	values/ScmBoolean.java	\
	machine/ContinuationFunction.java	\
	machine/WindContinuation.java	\
	machine/Continuation.java	\
	machine/Literal.java	\
	machine/Machine.java	\
	machine/Registers.java	\
	Translator.java	\
	functions/CheckedFunction.java	\
	functions/Reducer.java	\
	functions/MemvFunction.java	\
	functions/UnaryValueFunction.java	\
	functions/ValueThunk.java	\
	functions/DynamicWindFunction.java	\
	functions/AssocFunction.java	\
	functions/ParseBuiltins.java	\
	functions/AssqFunction.java	\
	functions/MemberBase.java	\
	functions/BinaryFunction.java	\
	functions/AppendFunction.java	\
	functions/BinaryValueFunction.java	\
	functions/AssocBase.java	\
	functions/Thunk.java	\
	functions/MemqFunction.java	\
	functions/ApplyFunction.java	\
	functions/TernaryValueFunction.java	\
	functions/TernaryFunction.java	\
	functions/MemberFunction.java	\
	functions/CallCCFunction.java	\
	functions/BuiltinTable.java	\
	functions/UnaryFunction.java	\
	functions/AssvFunction.java	\
	functions/Builtins.java	\
	syntax/Begin.java	\
	syntax/Set.java	\
	syntax/SyntaxFactory.java	\
	syntax/Lambda.java	\
	syntax/Letrec.java	\
	syntax/Quote.java	\
	syntax/Assign.java	\
	syntax/DefineSyntax.java	\
	syntax/LetStar.java	\
	syntax/Define.java	\
	syntax/Let.java	\
	syntax/If.java	\
	Main.java	\
	environment/StaticEnvironment.java	\
	environment/Reference.java	\
	environment/Environment.java	\
	code/Application.java	\
	code/Selection.java	\
	code/Sequence.java	\
	code/CodeList.java	\
	code/Assignment.java	\
	code/CompiledLambda.java	\
	util/Arity.java	\
	Value.java	\
	Code.java	\
	exceptions/FunctionExpected.java	\
	exceptions/ImmutableException.java	\
	exceptions/SymbolExpected.java	\
	exceptions/StringException.java	\
	exceptions/UnimplementedException.java	\
	exceptions/AlreadyBound.java	\
	exceptions/ListSyntaxException.java	\
	exceptions/ReadException.java	\
	exceptions/WriteException.java	\
	exceptions/SymbolNotFoundException.java	\
	exceptions/InvalidStringIndexException.java	\
	exceptions/VectorExpected.java	\
	exceptions/EnvironmentExpected.java	\
	exceptions/ParseException.java	\
	exceptions/PairExpected.java	\
	exceptions/OutputPortExpected.java	\
	exceptions/PortExpected.java	\
	exceptions/DuplicateSymbolException.java	\
	exceptions/UninitializedVectorException.java	\
	exceptions/TypeError.java	\
	exceptions/UnexpectedSyntax.java	\
	exceptions/CharExpected.java	\
	exceptions/NumberExpected.java	\
	exceptions/SchemeException.java	\
	exceptions/UninitializedSymbolException.java	\
	exceptions/SchemeIOException.java	\
	exceptions/FunctionNotFoundException.java	\
	exceptions/RuntimeError.java	\
	exceptions/VectorException.java	\
	exceptions/StringExpected.java	\
	exceptions/SyntaxArityError.java	\
	exceptions/CantCompileException.java	\
	exceptions/ListExpected.java	\
	exceptions/OpenException.java	\
	exceptions/CompileError.java	\
	exceptions/CloseException.java	\
	exceptions/RuntimeArityError.java	\
	exceptions/InvalidVectorIndexException.java	\
	exceptions/InputPortExpected.java	\
	Syntax.java

OBJ =	$(SRC:.java=.o)

SRC_TEST = 	TestFunction.java		\
	TestMachine.java			\
	TestR5RS.java				\
	environment/TestEnvironment.java	\
	values/TestInputPort.java		\
	values/TestList.java			\
	values/TestValue.java			\
	util/TestArity.java			\
	TestMScheme.java			\

PACKAGES =	MScheme			\
		MScheme.code		\
		MScheme.environment	\
		MScheme.exceptions	\
		MScheme.functions	\
		MScheme.machine		\
		MScheme.syntax		\
		MScheme.util		\
		MScheme.values

JIKES=jikes -bootclasspath /usr/lib/java/jre/lib/rt.jar
GCJ=gcj

all: functions/BuiltinTable.java
	$(JIKES) +P `find . -name "*.java"`
	time java junit.textui.TestRunner MScheme.TestMScheme

functions/BuiltinTable.java: functions/ParseBuiltins.class functions/Builtins.class
	java MScheme.functions.ParseBuiltins > functions/BuiltinTable.java

functions/ParseBuiltins.class: functions/ParseBuiltins.java
	$(JIKES) functions/ParseBuiltins.java

functions/Builtins.class: functions/Builtins.java
	$(JIKES) functions/Builtins.java

%.o: %.java
	$(GCJ) -ggdb -o $@ -c $<

main: $(OBJ)
	gcj -ggdb --main=MScheme.Main -o main $^

depend:
	touch depend

clean:
	rm -f $(OBJ)

doc:
	if [ \! -e doc ]; then mkdir doc; fi
	javadoc -d doc $(PACKAGES)

.PHONY: doc clean all

include depend
