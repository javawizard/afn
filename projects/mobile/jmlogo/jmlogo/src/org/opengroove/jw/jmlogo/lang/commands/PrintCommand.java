package org.opengroove.jw.jmlogo.lang.commands;

import org.opengroove.jw.jmlogo.lang.Command;
import org.opengroove.jw.jmlogo.lang.InterpreterContext;
import org.opengroove.jw.jmlogo.lang.Token;
import org.opengroove.jw.jmlogo.lang.WordToken;

public class PrintCommand extends Command
{
    
    public int getArgumentCount()
    {
        return 1;
    }
    
    public Token run(InterpreterContext context, Token[] arguments)
    {
        if (arguments[0] instanceof WordToken)
            System.out.println(((WordToken) arguments[0]).getValue());
        else
            System.out.println("Print called, TODO: finish the print command");
        return null;
    }
    
    public String getName()
    {
        return "print";
    }
    
}
