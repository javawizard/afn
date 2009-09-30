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
        int first = 3;
        int second = 5;
        Operation operation = Operation.multiply;
        String mode = "s";
        if (args.length > 0)
            mode = args[0];
        
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
