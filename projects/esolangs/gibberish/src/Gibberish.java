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
            
            index += 1;
        }
    }
}
