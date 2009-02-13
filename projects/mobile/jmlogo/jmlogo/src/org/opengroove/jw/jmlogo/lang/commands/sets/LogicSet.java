package org.opengroove.jw.jmlogo.lang.commands.sets;

import java.util.Vector;

import org.opengroove.jw.jmlogo.lang.Command;
import org.opengroove.jw.jmlogo.lang.InterpreterContext;
import org.opengroove.jw.jmlogo.lang.NamedCommand;
import org.opengroove.jw.jmlogo.lang.Token;
import org.opengroove.jw.jmlogo.lang.WordToken;

public class LogicSet
{
    public static Command[] commands = loadCommands();
    
    private static Command[] loadCommands()
    {
        Vector list = new Vector();
        list.addElement(new NamedCommand("not", 1, 1)
        {
            
            public Token run(InterpreterContext context, Token[] arguments)
            {
                validateWord(arguments[0]);
                WordToken word = (WordToken) arguments[0];
                return new WordToken(!word.getBool());
            }
        });
        list.addElement(new NamedCommand("lessp", 2, 2)
        {
            
            public Token run(InterpreterContext context, Token[] arguments)
            {
                verifyWords(arguments, 0, 2);
                WordToken w1 = (WordToken) arguments[0];
                WordToken w2 = (WordToken) arguments[1];
                return new WordToken(w1.getNumeric() < w2.getNumeric());
            }
        });
        list.addElement(new NamedCommand("greaterp", 2, 2)
        {
            
            public Token run(InterpreterContext context, Token[] arguments)
            {
                verifyWords(arguments, 0, 2);
                WordToken w1 = (WordToken) arguments[0];
                WordToken w2 = (WordToken) arguments[1];
                return new WordToken(w1.getNumeric() > w2.getNumeric());
            }
        });
        list.addElement(new NamedCommand("emptyp", 1, 1)
        {
            
            public Token run(InterpreterContext context, Token[] arguments)
            {
                // TODO Auto-generated method stub
                return null;
            }
        });
        list.addElement(new NamedCommand("equalp", 2, 2)
        {
            
            public Token run(InterpreterContext context, Token[] arguments)
            {
                // TODO Auto-generated method stub
                return null;
            }
        });
        list.addElement(new NamedCommand("and", 2, 1024)
        {
            
            public Token run(InterpreterContext context, Token[] arguments)
            {
                // TODO Auto-generated method stub
                return null;
            }
        });
        list.addElement(new NamedCommand("or", 2, 1024)
        {
            
            public Token run(InterpreterContext context, Token[] arguments)
            {
                // TODO Auto-generated method stub
                return null;
            }
        });
        Command[] them = new Command[list.size()];
        list.copyInto(them);
        return them;
    }
}
