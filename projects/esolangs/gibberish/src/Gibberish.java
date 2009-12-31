import java.io.FileInputStream;
import java.io.IOException;
import java.math.MathContext;
import java.util.Deque;
import java.util.Scanner;
import java.util.Stack;
import java.util.concurrent.LinkedBlockingDeque;

public class Gibberish
{
    public static Deque<Object> stack = new LinkedBlockingDeque<Object>();
    
    public static MathContext mc = new MathContext(300);
    
    public static int instructionSet;
    
    /**
     * @param args
     */
    public static void main(String[] args) throws IOException
    {
        if (args.length == 0)
        {
            System.out.println("Usage: gibberish <filename> -- Runs the specified file, "
                    + "as gibberish code. Input will be provided via stdin; "
                    + "output will be sent to stdout.");
        }
        FileInputStream in = new FileInputStream(args[0]);
        String code = new Scanner(in).useDelimiter("\\z").next();
        run(code);
    }
    
    private static void run(String code)
    {
        int index = 0;
        while (index < code.length())
        {
            index = process(index, code);
            index += 1;
        }
    }
    
    private static int process(int index, String code)
    {
        char c = code.charAt(index);
        int newIndex = processCommon(c, index, code);
        if (newIndex != -1)
            return newIndex;
        if (instructionSet == 1)
            return processFirst(c, index, code);
        else if (instructionSet == 2)
            return processSecond(c, index, code);
        else if (instructionSet == 3)
            return processThird(c, index, code);
        else
            throw new RuntimeException("The current instruction set was not 1, 2, or 3, "
                    + "and an instruction-set-specific instruction was executed.");
    }
    
    private static int processFirst(char c, int index, String code)
    {
        return index;
    }
    
    private static int processSecond(char c, int index, String code)
    {
        return index;
    }
    
    private static int processThird(char c, int index, String code)
    {
        return index;
    }
    
    private static int processCommon(char c, int index, String code)
    {
        if (c == '[')
        {
            
        }
        else
            return -1;
        return index;
    }
}
