package com.googlecode.jwutils.arithmetic;

import com.googlecode.jwutils.arithmetic.ExpressionParser.Token.Type;

public class ExpressionParser
{
    protected static class TokenStream
    {
        private Token[] tokens;
        private int index;
        
        public TokenStream(Token[] tokens)
        {
            this.tokens = tokens;
        }
        
        public boolean hasNext()
        {
            return index < tokens.length;
        }
        
        public Token next()
        {
            return tokens[index++];
        }
        
        public void rollback()
        {
            index--;
        }
    }
    
    protected static class Token
    {
        public Token(Type type, String operator, int number)
        {
            super();
            this.type = type;
            this.operator = operator;
            this.number = number;
        }
        
        private Type type;
        
        public static enum Type
        {
            NUMBER, OPERATOR, OPEN, CLOSE
        }
        
        private String operator;
        private int number;
        
        public Type getType()
        {
            return type;
        }
        
        public String getOperator()
        {
            return operator;
        }
        
        public int getNumber()
        {
            return number;
        }
    }
    
    /**
     * @param args
     */
    public static void main(String[] args)
    {
    }
    
    public static int parse(TokenStream tokens)
    {
        if (!tokens.hasNext())
            throw new IllegalArgumentException("not enough tokens");
        Token token = tokens.next();
        while (tokens.hasNext())
        {
            
        }
        return 0;
    }
    
}
