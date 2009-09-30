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
        BufferedImage image = Flashcards.createFlashcardFrontImage(3, 5,
                Operation.multiply);
        Flashcards.showImageInFrame(image);
    }
    
}
