# $Id$

SRC =	Code.java	\
	List.java	\
	Main.java	\
	Syntax.java	\
	Translator.java	\
	Value.java	\
	code/Application.java	\
	code/Assignment.java	\
	code/CodeList.java	\
	code/CompiledLambda.java	\
	code/Literal.java	\
	code/Selection.java	\
	code/Sequence.java	\
	environment/Environment.java	\
	environment/Reference.java	\
	environment/StaticEnvironment.java	\
	exceptions/AlreadyBound.java	\
	exceptions/CantCompileException.java	\
	exceptions/CharExpected.java	\
	exceptions/CloseException.java	\
	exceptions/CompileError.java	\
	exceptions/DuplicateSymbolException.java	\
	exceptions/EnvironmentExpected.java	\
	exceptions/FunctionExpected.java	\
	exceptions/FunctionNotFoundException.java	\
	exceptions/ImmutableException.java	\
	exceptions/InputPortExpected.java	\
	exceptions/InvalidStringIndexException.java	\
	exceptions/InvalidVectorIndexException.java	\
	exceptions/ListExpected.java	\
	exceptions/ListSyntaxException.java	\
	exceptions/NumberExpected.java	\
	exceptions/OpenException.java	\
	exceptions/OutputPortExpected.java	\
	exceptions/PairExpected.java	\
	exceptions/ParseException.java	\
	exceptions/PortExpected.java	\
	exceptions/ReadException.java	\
	exceptions/RuntimeArityError.java	\
	exceptions/RuntimeError.java	\
	exceptions/SchemeException.java	\
	exceptions/SchemeIOException.java	\
	exceptions/StringException.java	\
	exceptions/StringExpected.java	\
	exceptions/SymbolExpected.java	\
	exceptions/SymbolNotFoundException.java	\
	exceptions/SyntaxArityError.java	\
	exceptions/TypeError.java	\
	exceptions/UnexpectedSyntax.java	\
	exceptions/UnimplementedException.java	\
	exceptions/UninitializedSymbolException.java	\
	exceptions/UninitializedVectorException.java	\
	exceptions/VectorException.java	\
	exceptions/VectorExpected.java	\
	exceptions/WriteException.java	\
	functions/AppendFunction.java	\
	functions/ApplyFunction.java	\
	functions/AssocBase.java	\
	functions/AssocFunction.java	\
	functions/AssqFunction.java	\
	functions/AssvFunction.java	\
	functions/BinaryFunction.java	\
	functions/BinaryValueFunction.java	\
	functions/BuiltinTable.java	\
	functions/Builtins.java	\
	functions/CallCCFunction.java	\
	functions/CheckedFunction.java	\
	functions/DynamicWindFunction.java	\
	functions/MemberBase.java	\
	functions/MemberFunction.java	\
	functions/MemqFunction.java	\
	functions/MemvFunction.java	\
	functions/ParseBuiltins.java	\
	functions/Reducer.java	\
	functions/TernaryFunction.java	\
	functions/TernaryValueFunction.java	\
	functions/Thunk.java	\
	functions/UnaryFunction.java	\
	functions/UnaryValueFunction.java	\
	functions/ValueThunk.java	\
	functions/YCombinator.java	\
	machine/Continuation.java	\
	machine/ContinuationFunction.java	\
	machine/Machine.java	\
	machine/Registers.java	\
	machine/Result.java	\
	machine/WindContinuation.java	\
	syntax/Begin.java	\
	syntax/Define.java	\
	syntax/DefineSyntax.java	\
	syntax/If.java	\
	syntax/Lambda.java	\
	syntax/Let.java	\
	syntax/LetStar.java	\
	syntax/Letrec.java	\
	syntax/Quote.java	\
	syntax/Set.java	\
	syntax/SyntaxFactory.java	\
	util/Arity.java	\
	values/Compound.java	\
	values/Empty.java	\
	values/Function.java	\
	values/InputPort.java	\
	values/ListFactory.java	\
	values/OutputPort.java	\
	values/Pair.java	\
	values/PairOrList.java	\
	values/ScmBoolean.java	\
	values/ScmChar.java	\
	values/ScmNumber.java	\
	values/ScmString.java	\
	values/ScmVector.java	\
	values/Symbol.java	\
	values/ValueDefaultImplementations.java


OBJ =	$(SRC:.java=.o)

SRC_TEST =	TestMScheme.java	\
		TestMachine.java	\
		TestR5RS.java	\
		environment/TestEnvironment.java	\
		util/TestArity.java	\
		values/TestInputPort.java	\
		values/TestList.java	\
		tests/TestValue.java

PACKAGES =	MScheme			\
		MScheme.code		\
		MScheme.environment	\
		MScheme.exceptions	\
		MScheme.functions	\
		MScheme.machine		\
		MScheme.syntax		\
		MScheme.util		\
		MScheme.values		\
		MScheme.tests

JIKES=jikes -bootclasspath /usr/lib/java/jre/lib/rt.jar
GCJ=gcj

all: functions/BuiltinTable.java
	$(JIKES) +P `find . -name "*.java"`
	time java junit.textui.TestRunner MScheme.TestMScheme

functions/BuiltinTable.java: functions/ParseBuiltins.class functions/Builtins.class
	java MScheme.functions.ParseBuiltins > functions/BuiltinTable.java

functions/ParseBuiltins.class: functions/ParseBuiltins.java
	cp functions/BuiltinTable.java.empty functions/BuiltinTable.java
	$(JIKES) functions/ParseBuiltins.java
	touch functions/Builtins.java

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
	javadoc -private -d doc $(PACKAGES)

.PHONY: doc clean all

include depend
