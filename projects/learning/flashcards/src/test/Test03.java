package test;

import java.awt.image.BufferedImage;

import jw.flashcards.Flashcards;
import jw.flashcards.Flashcards.Operation;

public class Test03
{
    
    /**
     * @param args
     */
    public static void main(String[] args)
    {
        int first = 3;
        int second = 5;
        Operation operation = Operation.multiply;
        
        BufferedImage image = Flashcards.createFlashcardFrontImage(first,
                second, operation);
        Flashcards.showImageInFrame(image);
        BufferedImage image2 = Flashcards.createFlashcardBackImage(first,
                second, operation);
        Flashcards.showImageInFrame(image2);
    }
    
}
