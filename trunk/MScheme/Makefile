# $Id$

SRC =	Code.java	\
	Main.java	\
	Syntax.java	\
	Value.java	\
	Init.java	\
	code/Application.java	\
	code/ApplyContinuation.java	\
	code/Assignment.java	\
	code/CompiledLambda.java	\
	code/Selection.java	\
	code/Sequence.java	\
	code/CodeArray.java	\
	environment/Environment.java	\
	environment/Reference.java	\
	environment/DynamicEnvironment.java	\
	environment/StaticEnvironment.java	\
	exceptions/AlreadyBound.java	\
	exceptions/CantCompileException.java	\
	exceptions/CharExpected.java	\
	exceptions/CloseException.java	\
	exceptions/CompileError.java	\
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
	syntax/LetBase.java	\
	syntax/Let.java	\
	syntax/LetStar.java	\
	syntax/Letrec.java	\
	syntax/Quote.java	\
	syntax/ProcedureCall.java	\
	syntax/Set.java	\
	syntax/SequenceTags.java	\
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
                tests/TestSchemeBase.java	\
		tests/TestMachine.java	\
		tests/TestR5RS.java	\
		tests/TestBugs.java	\
		environment/TestEnvironment.java	\
		tests/TestValue.java	\
		util/TestArity.java	\
		values/TestInputPort.java	\
		values/TestList.java

PACKAGES =	MScheme				\
		MScheme.values			\
		MScheme.values.functions	\
		MScheme.machine			\
		MScheme.util			\
		MScheme.exceptions		\
		MScheme.environment		\
		MScheme.syntax			\
		MScheme.code			\
		MScheme.tests

JAVAC=/usr/lib/j2sdk1.4.1_01/bin/javac
#JAVAC=jikes +P -bootclasspath /usr/lib/jdk1.1/lib/classes.zip
#JAVAC=gcj -v -W -Wall -C

JAVA=/usr/lib/j2sdk1.4.1_01/bin/java


run: all
	time $(JAVA) MScheme.TestMScheme

all: Init.java values/functions/BuiltinTable.java
	$(JAVAC) `find . -name "*.java"`

values/functions/BuiltinTable.java: values/functions/ParseBuiltins.java values/functions/Builtins.java
	cp values/functions/BuiltinTable.java.empty values/functions/BuiltinTable.java
	$(JAVAC) values/functions/ParseBuiltins.java
	$(JAVA) MScheme.values.functions.ParseBuiltins > values/functions/BuiltinTable.java
	$(JAVAC) values/functions/BuiltinTable.java

Init.java: bootstrap.scm rep.scm
	./mkinit.pl $^ > $@

doc:
	if [ \! -e doc ]; then mkdir doc; fi
	javadoc -private -d doc $(PACKAGES)

.PHONY: doc clean all
