#!/bin/bash

javac com/craftinginterpreters/lox/Lox.java
java --enable-preview -XX:+ShowCodeDetailsInExceptionMessages com.craftinginterpreters.lox.Lox;
