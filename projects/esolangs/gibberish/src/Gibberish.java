import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.util.Deque;
import java.util.Iterator;
import java.util.Scanner;
import java.util.Stack;
import java.util.concurrent.LinkedBlockingDeque;

public class Gibberish
{
    public static Deque<Object> stack = new LinkedBlockingDeque<Object>();
    
    public static MathContext mc = new MathContext(300);
    
    public static int instructionSet;
    
    public static BufferedReader input;
    
    public static boolean debug = false;
    
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
            return;
        }
        input = new BufferedReader(new InputStreamReader(System.in));
        FileInputStream in = new FileInputStream(args[0]);
        String code = new Scanner(in).useDelimiter("\\z").next();
        run(code);
    }
    
    private static void run(String code) throws IOException
    {
        int index = 0;
        while (index < code.length())
        {
            index = process(index, code);
            index += 1;
        }
    }
    
    private static int process(int index, String code) throws IOException
    {
        char c = code.charAt(index);
        if (c == ' ' || c == '\n' || c == '\r' || c == '\t')
            // ignoring whitespace for now
            return index;
        debug("Set " + instructionSet + " command " + c + ", stack is (top to bottom) "
                + debugStack());
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
    
    private static String debugStack()
    {
        Object[] os = stack.toArray();
        StringBuffer result = new StringBuffer();
        for (Object o : os)
        {
            result.append(", ");
            result.append((o instanceof BigDecimal) ? o.toString() : "\"" + o.toString()
                    + "\"");
        }
        if (result.length() == 0)
            return "(empty)";
        else
            return result.toString().substring(2);
    }
    
    private static int processFirst(char c, int index, String code) throws IOException
    {
        if (c == 'a')
        {
            stack.push(((BigDecimal) stack.pop()).add((BigDecimal) stack.pop()));
        }
        else if (c == 'i')
        {
            stack.push(new BigDecimal((String) stack.pop()));
        }
        else if (c == 'l')
        {
            stack.push(input.readLine());
        }
        else if (c == 'o')
        {
            System.out.println(stack.pop());
        }
        else if (c == 'p')
        {
            int pos = ((BigDecimal) stack.pop()).intValue();
            Iterator it = stack.iterator();
            for (int i = 0; i < pos; i++)
                it.next();
            Object o = it.next();
            stack.push(o);
        }
        else if (c == 'q')
        {
            System.out.print(stack.pop());
            System.out.flush();
        }
        else if (c == 'u')
        {
            Object o = stack.pop();
            stack.push(o);
            stack.push(o);
        }
        else if (c == 'v')
        {
            stack.pop();
        }
        else
        {
            invalidInstruction(c);
        }
        return index;
    }
    
    private static int processSecond(char c, int index, String code)
    {
        if (c == 'n')
        {
            stack.push((((BigDecimal) stack.pop()).intValue() == 1) ? BigDecimal.ZERO
                    : BigDecimal.ONE);
        }
        else if (c == 'q')
        {
            stack.push(stack.pop().equals(stack.pop()) ? BigDecimal.ONE : BigDecimal.ZERO);
        }
        else
        {
            invalidInstruction(c);
        }
        return index;
    }
    
    private static int processThird(char c, int index, String code) throws IOException
    {
        if (c == 'w')
        {
            String loop = (String) stack.pop();
            while (((BigDecimal) stack.pop()).intValue() == 1)
            {
                debug("running loop at " + index);
                run(loop);
            }
        }
        else
        {
            invalidInstruction(c);
        }
        return index;
    }
    
    private static int processCommon(char c, int index, String code)
    {
        if (c == '[')
        {
            index += 1;
            int level = 0;
            StringBuffer result = new StringBuffer();
            while (code.charAt(index) != ']' || level != 0)
            {
                char current = code.charAt(index);
                if (current == '[')
                    level += 1;
                else if (current == ']')
                    level -= 1;
                result.append(current);
                index += 1;
            }
            stack.push(result.toString());
        }
        else if (c >= '0' && c <= '9')
        {
            stack.push(new BigDecimal(c - '0'));
        }
        else if (c >= 'e' && c <= 'g')
        {
            instructionSet = (c - 'e') + 1;
        }
        else if (c == 'j')
        {
            stack.push(new BigDecimal(instructionSet));
        }
        else if (c == 'x')
        {
            instructionSet = ((BigDecimal) stack.pop()).intValue();
            if (instructionSet < 0 || instructionSet > 3)
                throw new RuntimeException("The \"x\" instruction set the "
                        + "instruction set number to " + instructionSet
                        + ", which isn't valid. It can only be 0, 1, 2, or 3.");
        }
        else if (c == 'z')
        {
            // nothing to do here
        }
        else
            return -1;
        return index;
    }
    
    private static void invalidInstruction(char c)
    {
        throw new RuntimeException("The instruction \"" + c
                + "\" is not a valid instruction in instruction set " + instructionSet
                + ".");
    }
    
    public static void debug(String s)
    {
        if (debug)
            System.out.println(s);
    }
}
