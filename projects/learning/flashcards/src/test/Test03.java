package test;

import java.awt.image.BufferedImage;

import jw.flashcards.Flashcards;
import jw.flashcards.Flashcards.Operation;

public class Test03
{
    
    /**
     * @param args
     */
    public static void main(String[] args) throws Throwable
    {
        int first = Integer.parseInt(args[0]);
        int second = Integer.parseInt(args[2]);
        Operation operation = Operation.valueOf(args[1]);
        String mode = "s";
        if (args.length > 3)
            mode = args[3];
        
        BufferedImage front = Flashcards.createFlashcardFrontImage(first,
                second, operation);
        BufferedImage back = Flashcards.createFlashcardBackImage(first, second,
                operation);
        if (mode.equals("s"))
        {
            Flashcards.showImageInFrame(back);
            Flashcards.showImageInFrame(front);
        }
        else if (mode.equals("f"))
        {
            Flashcards.printImage(front);
        }
        else if (mode.equals("b"))
        {
            Flashcards.printImage(back);
        }
    }
    
}
