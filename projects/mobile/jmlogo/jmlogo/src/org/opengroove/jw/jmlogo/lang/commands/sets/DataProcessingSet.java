package org.opengroove.jw.jmlogo.lang.commands.sets;

import org.opengroove.jw.jmlogo.lang.Command;
import org.opengroove.jw.jmlogo.lang.InterpreterContext;
import org.opengroove.jw.jmlogo.lang.ListToken;
import org.opengroove.jw.jmlogo.lang.NamedCommand;
import org.opengroove.jw.jmlogo.lang.Token;
import org.opengroove.jw.jmlogo.lang.WordToken;
import org.opengroove.jw.jmlogo.lang.InterpreterException;

public class DataProcessingSet
{
    public static final Command[] set =
        new Command[] { new NamedCommand("list", 2, 256)
        {
            
            public Token run(InterpreterContext context, Token[] arguments)
            {
                return new ListToken(arguments);
            }
        }, new NamedCommand("wraplist", 1, 1)
        {
            
            public Token run(InterpreterContext context, Token[] arguments)
            {
                return new ListToken(arguments);
            }
        }, new NamedCommand("word", 2, 256)
        {
            
            public Token run(InterpreterContext context, Token[] arguments)
            {
                StringBuffer buf = new StringBuffer();
                for (int i = 0; i < arguments.length; i++)
                {
                    if (!(arguments[i] instanceof WordToken))
                        throw new InterpreterException(
                            "inputs to the word command must themselves be words");
                    buf.append(((WordToken) arguments[i]).getValue());
                }
                return new WordToken(buf.toString());
            }
        }, new NamedCommand("fput", 2, 2)
        {
            
            public Token run(InterpreterContext context, Token[] arguments)
            {
                validateList(arguments[1]);
                ListToken source = (ListToken) arguments[1];
                return source.fput(arguments[0]);
            }
        }, new NamedCommand("lput", 2, 2)
        {
            
            public Token run(InterpreterContext context, Token[] arguments)
            {
                validateList(arguments[1]);
                ListToken source = (ListToken) arguments[1];
                return source.lput(arguments[1]);
            }
        }, new NamedCommand("pop", 1, 1)
        {
            
            public Token run(InterpreterContext context, Token[] arguments)
            {
                validateWord(arguments[0]);
                WordToken token = (WordToken) arguments[0];
                String tokenValue = token.getValue();
                Token list = context.getVariable(tokenValue);
                validateList(list);
                ListToken listToken = (ListToken) list;
                if (listToken.getMembers().length < 1)
                    throw new InterpreterException(
                        "You can't pop from a stack that doesn't have anything on it");
                Token value = listToken.getMembers()[0];
                Token[] newTokens = new Token[listToken.getMembers().length];
                System.arraycopy(listToken.getMembers(), 1, newTokens, 0,
                    newTokens.length);
                ListToken newList = new ListToken(newTokens);
                context.setVariable(tokenValue, newList);
                return value;
            }
        }, new NamedCommand("push", 2, 2)
        {
            
            public Token run(InterpreterContext context, Token[] arguments)
            {
                
            }
        } };
}
