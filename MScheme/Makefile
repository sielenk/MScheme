# $Id$

SRC =	Code.java	\
	Main.java	\
	Syntax.java	\
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
	exceptions/ImmutableException.java	\
	exceptions/InputPortExpected.java	\
	exceptions/InvalidStringIndexException.java	\
	exceptions/InvalidVectorIndexException.java	\
	exceptions/ListExpected.java	\
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
	exceptions/UninitializedSymbolException.java	\
	exceptions/UninitializedVectorException.java	\
	exceptions/VectorException.java	\
	exceptions/VectorExpected.java	\
	exceptions/WriteException.java	\
	values/functions/AppendFunction.java	\
	values/functions/ApplyFunction.java	\
	values/functions/AssocBase.java	\
	values/functions/AssocFunction.java	\
	values/functions/AssqFunction.java	\
	values/functions/AssvFunction.java	\
	values/functions/BinaryFunction.java	\
	values/functions/BinaryValueFunction.java	\
	values/functions/BuiltinTable.java	\
	values/functions/Builtins.java	\
	values/functions/CallCCFunction.java	\
	values/functions/CheckedFunction.java	\
	values/functions/DynamicWindFunction.java	\
	values/functions/EvalFunction.java	\
	values/functions/MemberBase.java	\
	values/functions/MemberFunction.java	\
	values/functions/MemqFunction.java	\
	values/functions/MemvFunction.java	\
	values/functions/Reducer.java	\
	values/functions/SpawnFunction.java	\
	values/functions/TernaryFunction.java	\
	values/functions/TernaryValueFunction.java	\
	values/functions/Thunk.java	\
	values/functions/UnaryFunction.java	\
	values/functions/UnaryValueFunction.java	\
	values/functions/ValueThunk.java	\
	values/functions/YCombinator.java	\
	machine/Continuation.java	\
	machine/ContinuationFunction.java	\
	machine/Controller.java	\
	machine/Machine.java	\
	machine/Registers.java	\
	machine/Result.java	\
	machine/WindContinuation.java	\
	syntax/Begin.java	\
	syntax/CheckedSyntax.java	\
	syntax/Define.java	\
	syntax/DefineSyntax.java	\
	syntax/If.java	\
	syntax/Lambda.java	\
	syntax/Let.java	\
	syntax/LetStar.java	\
	syntax/Letrec.java	\
	syntax/Quote.java	\
	syntax/ProcedureCall.java	\
	syntax/Set.java	\
	syntax/SyntaxFactory.java	\
	util/Arity.java	\
	values/Compound.java	\
	values/Empty.java	\
	values/Function.java	\
	values/InputPort.java	\
	values/ListFactory.java	\
	values/List.java	\
	values/OutputPort.java	\
	values/Pair.java	\
	values/PairOrList.java	\
	values/Port.java	\
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
		tests/TestValue.java	\
		util/TestArity.java	\
		values/TestInputPort.java	\
		values/TestList.java

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

GCJ=gcj -W -Wall
JAVAC=jikes -bootclasspath /usr/lib/java/jre/lib/rt.jar +P
# JAVAC = $(GCJ) -W -Wall -C

all: values/functions/BuiltinTable.java
	$(JAVAC) `find . -name "*.java"`
	time java MScheme.TestMScheme

values/functions/BuiltinTable.java: values/functions/ParseBuiltins.class values/functions/Builtins.class
	java MScheme.values.functions.ParseBuiltins > values/functions/BuiltinTable.java

values/functions/ParseBuiltins.class: values/functions/ParseBuiltins.java
	cp values/functions/BuiltinTable.java.empty values/functions/BuiltinTable.java
	$(JAVAC) values/functions/ParseBuiltins.java
	touch values/functions/Builtins.java

values/functions/Builtins.class: values/functions/Builtins.java
	$(JAVAC) values/functions/Builtins.java

%.o: %.java
	$(GCJ) -o $@ -c $<

main: $(OBJ)
	$(GCJ) --main=MScheme.Main -o main $^

depend:
	touch depend

clean:
	rm -f $(OBJ)

indent:
	for i in `find . -name "*.java"`; do		\
		echo $$i;				\
		astyle -j -s -b --convert-tabs $$i;	\
	done

doc:
	if [ \! -e doc ]; then mkdir doc; fi
	javadoc -private -d doc $(PACKAGES)

.PHONY: doc clean all indent

include depend
