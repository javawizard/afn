package afn.parcon;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;
import java.util.Stack;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * This class provides utilities to help with parsing grammars that use the
 * off-side rule, i.e. block scope is determined by indentation.<br/>
 * <br/>
 * 
 * This class and the utilities it provides are somewhat hackish, and I hope to
 * create a replacement with a better design soon.
 * 
 * @author jcp
 * 
 */
public class Offside {
    /**
     * The one-character string representing an indentation. This is currently
     * Unicode character 0xFDE0, which is a non-character Unicode symbol.
     */
    public static final String INDENT = "\ufde0";
    /**
     * The one-character string representing a dedentation. This is currently
     * Unicode character 0xFDE1, which is a non-character Unicode symbol.
     */
    public static final String DEDENT = "\ufde1";
    
    private static final Pattern NOT_WHITESPACE = Pattern.compile("[^ \t\r\n]");
    
    /**
     * Parses through the specified input, removing leading whitespace on lines
     * and replacing starts of indentation with {@link #INDENT} and ends of
     * indentation with {@link #DEDENT}.
     * 
     * @param input
     * @return
     */
    public static String parseIndentation(String input) {
        /*
         * TODO: This really needs to be designed better. There are a number of
         * problems with the current solution:
         * 
         * It doesn't involve any actual Parcon parsers, so, for example,
         * comments aren't properly filtered out and strings will be completely
         * messed up
         * 
         * It has to apply itself to the entire input text at once, instead of
         * just the portions that the parser wants to know about
         */
        Stack<Integer> levels = new Stack<Integer>();
        levels.push(0);
        Scanner scanner = new Scanner(input);
        StringBuilder result = new StringBuilder();
        while (scanner.hasNextLine()) {
            String line = scanner.nextLine();
            if (line.trim().equals("")) // Skip empty lines
                continue;
            int topLevel = levels.peek();
            int currentLevel = getLeadingSpaces(line);
            if (currentLevel > topLevel) { // indent
                levels.push(currentLevel);
                result.append(INDENT);
            } else if (currentLevel < topLevel) { // dedent
                while (currentLevel < levels.peek()) {
                    // While the indentation level on the stack is greater than
                    // our actual level, pop the level on the stack and write a
                    // dedent
                    levels.pop();
                    result.append(DEDENT);
                }
            }
            result.append(line.substring(currentLevel) + "\n");
        }
        while (levels.peek() > 0) {
            // Write out any remaining dedents
            levels.pop();
            result.append(DEDENT);
        }
        return result.toString();
    }
    
    private static int getLeadingSpaces(String line) {
        Matcher m = NOT_WHITESPACE.matcher(line);
        m.find();
        return m.start();
    }
}
