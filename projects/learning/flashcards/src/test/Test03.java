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
        BufferedImage image = Flashcards.createFlashcardFrontImage(19, 13,
                Operation.subtract);
        Flashcards.showImageInFrame(image);
    }
    
}
