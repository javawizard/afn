@echo off
set /P testvar=Enter the number of the test you wish to run:   
java -cp classes;lib/* tests.Test%testvar%
