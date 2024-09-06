#!/bin/bash

mvn exec:java -Dexec.mainClass="com.craftinginterpreters.tool.GenerateAst" \
    -Dexec.args="src/main/java/com/craftinginterpreters/lox"
