package test;

import jw.jzbot.fact.FactContext;
import jw.jzbot.fact.FactEntity;
import jw.jzbot.fact.FactParser;

public class Test18
{
    
    /**
     * @param args
     */
    public static void main(String[] args)
    {
        String program = "Numbers from 1 to 10: {{split|| ||{{numberlist||1||10}}"
                + "||thenumber||%thenumber%||, }}";
        FactEntity entity = FactParser.parse(program, "a_test_program");
        FactContext context = new FactContext();
        System.out.println("Program output: " + entity.resolve(context));
    }
}
