all: functions/BuiltinTable.java
	jikes +P +M=depend `find . -name "*.java"`

functions/BuiltinTable.java: functions/ParseBuiltins.class functions/Builtins.class
	java MScheme.functions.ParseBuiltins > functions/BuiltinTable.java

functions/ParseBuiltins.class: functions/ParseBuiltins.java
	jikes functions/ParseBuiltins.java

functions/Builtins.class: functions/Builtins.java
	jikes functions/Builtins.java
