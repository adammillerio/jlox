#!/bin/bash

mvn -q exec:java \
    -Dexec.mainClass="com.craftinginterpreters.lox.Lox" -Dexec.arguments="${@}"
