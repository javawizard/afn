package org.opengroove.jw.jmlogo.lang.commands.sets;

import org.opengroove.jw.jmlogo.lang.Command;
import org.opengroove.jw.jmlogo.lang.InterpreterContext;
import org.opengroove.jw.jmlogo.lang.NamedCommand;
import org.opengroove.jw.jmlogo.lang.Token;
import org.opengroove.jw.jmlogo.lang.WordToken;
import org.opengroove.jw.jmlogo.lang.commands.math.OneArgMathCommand;
import org.opengroove.jw.jmlogo.utils.Math2;

public class MathSet
{
    public static Command[] commands = new Command[] { new OneArgMathCommand()
    {
        
        public double compute(double v)
        {
            return Math.sqrt(v);
        }
        
        public String getName()
        {
            return "sqrt";
        }
    }, new OneArgMathCommand()
    {
        
        public double compute(double v)
        {
            return Math2.round(v);
        }
        
        public String getName()
        {
            return "round";
        }
    }, new OneArgMathCommand()
    {
        
        public double compute(double v)
        {
            return Math.sin(Math.toRadians(v));
        }
        
        public String getName()
        {
            return "sin";
        }
    }, new OneArgMathCommand()
    {
        
        public double compute(double v)
        {
            // TODO Auto-generated method stub
            return 0;
        }
        
        public String getName()
        {
            // TODO Auto-generated method stub
            return "arcsin";
        }
    }, new OneArgMathCommand()
    {
        
        public double compute(double v)
        {
            // TODO Auto-generated method stub
            return 0;
        }
        
        public String getName()
        {
            // TODO Auto-generated method stub
            return "tan";
        }
    }, new OneArgMathCommand()
    {
        
        public double compute(double v)
        {
            // TODO Auto-generated method stub
            return 0;
        }
        
        public String getName()
        {
            // TODO Auto-generated method stub
            return "arctan";
        }
    }, new TwoArgMathCommand()
    {
        
        public double compute(double v1, double v2)
        {
            // TODO Auto-generated method stub
            return 0;
        }
        
        public String getName()
        {
            // TODO Auto-generated method stub
            return "arctan2";
        }
    } };
}
